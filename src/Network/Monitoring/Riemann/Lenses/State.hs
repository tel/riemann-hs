{-# LANGUAGE RankNTypes #-}

module Network.Monitoring.Riemann.Lenses.State (
  time, state, service, host, description, tags, ttl,
  once
  ) where

import Network.Monitoring.Riemann.Lenses.Class

import GHC.Int (Int64)

import Text.ProtocolBuffers (Utf8 (..))
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.Encoding as TE
import Data.Sequence (Seq, fromList)
import Data.Foldable (toList)

import qualified Network.Monitoring.Riemann.Proto.State as S
import qualified Network.Monitoring.Riemann.Proto.Attribute as A

toUtf8 :: Text -> Utf8
toUtf8 = Utf8 . TE.encodeUtf8

fromUtf8 :: Utf8 -> Text
fromUtf8 (Utf8 lbs) = TE.decodeUtf8 lbs

instance Stated S.State where
  time lift st =
    fmap (\v -> st {S.time = v}) $ lift (S.time st)
  state lift st =
    fmap (\v -> st {S.state = fmap toUtf8 v})
    $ lift (fmap fromUtf8 $ S.state st)
  service lift st =
    fmap (\v -> st {S.service = fmap toUtf8 v})
    $ lift (fmap fromUtf8 $ S.service st)
  host lift st =
    fmap (\v -> st {S.host = fmap toUtf8 v})
    $ lift (fmap fromUtf8 $ S.host st)
  description lift st =
    fmap (\v -> st {S.description = fmap toUtf8 v})
    $ lift (fmap fromUtf8 $ S.description st)
  tags lift st =
    fmap (\v -> st {S.tags = fromList $ map toUtf8 $ v})
    $ lift (map fromUtf8 $ toList $ S.tags st)
  ttl lift st =
    fmap (\v -> st {S.ttl = v}) $ lift (S.ttl st)

once :: forall f. Functor f =>
        ((Maybe Bool) -> f (Maybe Bool)) -> S.State -> f S.State
once lift st = fmap (\v -> st {S.once = v}) $ lift (S.once st)

