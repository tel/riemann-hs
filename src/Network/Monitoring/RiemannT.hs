{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns #-}

module Network.Monitoring.RiemannT where

import Network.Monitoring.Riemann.Lenses
import Network.Monitoring.Riemann

import Data.Functor.Identity
import Control.Monad.Trans
import Control.Monad.IO.Class
import Control.Monad
import Control.Applicative

import Control.Monad.Trans.Free

type Riemann a = RiemannT Identity a

data RiemannF a = RiemannF Event a deriving (Functor)

newtype RiemannT m a =
  RiemannT (FreeT RiemannF m a)
  deriving (Functor, Monad, Applicative, MonadTrans, MonadIO)

-- | Observes an 'Event' in the 'RiemannT' monad.
obs :: Monad m => Event -> RiemannT m ()
obs = RiemannT . liftF . (`RiemannF` ()) 

-- | 'runRiemannT c' is for any 'MonadIO m' a natural transformation
-- from 'RiemannT m' to 'm', delivering the events raised in 'RiemannT
-- m' to the 'Client' 'c'.
runRiemannT :: MonadIO m => Client -> RiemannT m a -> m a
runRiemannT client (RiemannT w) = runFreeT w >>= \v ->
  case v of
    Pure a -> return a
    Free (RiemannF ev next) -> do
      liftIO $ sendEvent client ev
      runRiemannT client (RiemannT next)

-- | Extracts the observed events from a 'Riemann' monad
observed :: Riemann a -> [Event]
observed = runIdentity . observedT

-- | Extracts the observed events from a 'RiemannT' monad
observedT :: Monad m => RiemannT m a -> m [Event]
observedT (RiemannT m) = runFreeT m >>= \v ->
  case v of
    Pure _ -> return []
    Free (RiemannF ev next) -> (ev:) `liftM` observedT (RiemannT next)