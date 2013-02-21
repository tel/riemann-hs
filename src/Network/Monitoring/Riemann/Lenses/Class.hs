{-# LANGUAGE RankNTypes #-}

module Network.Monitoring.Riemann.Lenses.Class (
  Stated (..)
  ) where

import GHC.Int (Int64)

import Data.Text.Lazy (Text)

-- | 'Stated' types are 'Event' and 'State' which both have
-- information representing the state of a 'service' on a 'host' at a
-- given 'time'. These shared types give rise to restrictedly
-- polymorphic lenses.
class Stated a where
  time        :: forall f. Functor f =>
                 ((Maybe Int64) -> f (Maybe Int64)) -> (a -> f a)
  state       :: forall f. Functor f =>
                 ((Maybe Text) -> f (Maybe Text)) -> (a -> f a)
  service     :: forall f. Functor f =>
                 ((Maybe Text) -> f (Maybe Text)) -> (a -> f a)
  host        :: forall f. Functor f =>
                 ((Maybe Text) -> f (Maybe Text)) -> (a -> f a)
  description :: forall f. Functor f =>
                 ((Maybe Text) -> f (Maybe Text)) -> (a -> f a)
  tags        :: forall f. Functor f =>
                 ([Text] -> f [Text]) -> (a -> f a)
  ttl         :: forall f. Functor f =>
                 ((Maybe Float) -> f (Maybe Float)) -> (a -> f a)