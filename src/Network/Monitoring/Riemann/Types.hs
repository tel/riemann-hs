{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.Monitoring.Riemann.Types (
  Event, State, Msg, Query,
  module Network.Monitoring.Riemann.Lenses
  ) where

import Network.Monitoring.Riemann.Proto.Event (Event)
import Network.Monitoring.Riemann.Proto.State (State)
import Network.Monitoring.Riemann.Proto.Msg   (Msg)
import Network.Monitoring.Riemann.Proto.Query (Query)

import Network.Monitoring.Riemann.Lenses

import Text.ProtocolBuffers.Header

import Data.Monoid
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

import Data.Functor.Identity
import Data.Functor.Constant
import Control.Monad.Trans.State (execState, modify)
import qualified Control.Monad.Trans.State as St

import Control.Applicative


-- mu-Lens

get :: (forall f. Functor f => (a -> f a) -> b -> f b) -> b -> a
get lens = getConstant . lens Constant

modi :: (forall f. Functor f => (a -> f a) -> b -> f b) -> (a -> a) -> b -> b
modi lens f = runIdentity . lens (Identity . f)

set :: (forall f. Functor f => (a -> f a) -> b -> f b) -> a -> b -> b
set lens b = modi lens (const b)

-- | Given some object, a lens acting on its type, and a method of
-- merging the focused types, execute a state action to precompose
-- this objects focused value into the state's object's focused value.
mergeFront ::
  b 
  -> (forall f. Functor f => (a -> f a) -> b -> f b)
  -> (a -> a -> a)
  -> St.State b ()
mergeFront o lens op = modify $ modi lens (op (get lens o))

-- | Stateful setting while wrapping into an `Applicative`.
(?=) :: Applicative m =>
        (forall f. Functor f => ((m a) -> f (m a)) -> b -> f b)
        -> a -> St.State b ()
lens ?= value = modify (set lens (pure value))


-- Nicer constructors

-- | Create a simple 'Event' with state "ok".
--
-- >>> get state $ ev "service" (0 :: Int64)
-- Just "ok"
--
-- >>> get service $ ev "service" (0 :: Int64)
-- Just "service"
--
-- >>> get metric $ ev "service" (0 :: Int64) :: Maybe Int64
-- Just 0
--
-- >>> get tags $ ev "service" (0 :: Int64)
-- []
ev :: Metricable a => String -> a -> Event
ev serv met =
  flip execState mempty $ do
    state   ?= T.pack "ok"
    service ?= T.pack serv
    metric  ?= met


-- Monoid instances

instance Monoid Event where
  mempty = defaultValue
  a `mappend` b = flip execState b $ do
    tags       >< mappend
    ttl        >< liftA2 max
    attributes >< mappend
    where (><) = mergeFront a

instance Monoid State where
  mempty = defaultValue
  a `mappend` b = flip execState b $ do
    ttl >< liftA2 max
    where (><) = mergeFront a