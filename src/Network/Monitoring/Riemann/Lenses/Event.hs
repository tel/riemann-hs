{-# LANGUAGE RankNTypes #-}

module Network.Monitoring.Riemann.Lenses.Event (
  time, state, service, host, description, tags, ttl,
  attributes,
  Metricable (..)
  ) where

import Network.Monitoring.Riemann.Lenses.Class

import GHC.Int (Int64)

import Data.Maybe (catMaybes)
import Text.ProtocolBuffers (Utf8 (..))
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.Encoding as TE
import Data.Sequence (Seq, fromList)
import Data.Foldable (toList)

import qualified Network.Monitoring.Riemann.Proto.Event as E
import qualified Network.Monitoring.Riemann.Proto.Attribute as A

toUtf8 :: Text -> Utf8
toUtf8 = Utf8 . TE.encodeUtf8

fromUtf8 :: Utf8 -> Text
fromUtf8 (Utf8 lbs) = TE.decodeUtf8 lbs

instance Stated E.Event where
  time lift ev =
    fmap (\v -> ev {E.time = v}) $ lift (E.time ev)
  state lift ev =
    fmap (\v -> ev {E.state = fmap toUtf8 v})
    $ lift (fmap fromUtf8 $ E.state ev)
  service lift ev =
    fmap (\v -> ev {E.service = fmap toUtf8 v})
    $ lift (fmap fromUtf8 $ E.service ev)
  host lift ev =
    fmap (\v -> ev {E.host = fmap toUtf8 v})
    $ lift (fmap fromUtf8 $ E.host ev)
  description lift ev =
    fmap (\v -> ev {E.description = fmap toUtf8 v})
    $ lift (fmap fromUtf8 $ E.description ev)
  tags lift ev =
    fmap (\v -> ev {E.tags = fromList $ map toUtf8 $ v})
    $ lift (map fromUtf8 $ toList $ E.tags ev)
  ttl lift ev =
    fmap (\v -> ev {E.ttl = v}) $ lift (E.ttl ev)

attributes :: forall f. Functor f =>
              ([(Text, Text)] -> f [(Text, Text)])
              -> E.Event -> f E.Event
attributes lift ev =
  fmap (\v -> ev {E.attributes = reAttrs v})
  $ lift (unAttrs $ E.attributes ev)
  where reAttrs :: [(Text, Text)] -> Seq A.Attribute
        reAttrs = fromList . map reAttr

        unAttrs :: Seq A.Attribute -> [(Text, Text)]
        unAttrs = catMaybes . map unAttr . toList

        reAttr (k, v) = A.Attribute (toUtf8 k) (Just $ toUtf8 v)
        unAttr (A.Attribute {A.key = k, A.value = vMay}) =
          case vMay of
            Nothing -> Nothing
            Just v  -> Just (fromUtf8 k, fromUtf8 v)
  
-- | 'Metricable' types are those which can be 'metric's in an
-- 'Event'. This class provides dispatch from Haskell's polymorphic
-- types to Protobuf's non-polymorphic types.
class Metricable a where
  metric :: forall f. Functor f =>
            ((Maybe a) -> f (Maybe a)) -> (E.Event -> f E.Event)

instance Metricable Int64 where
  metric lift ev =
    fmap (\v -> ev {E.metric_sint64 = v})
    $ lift (E.metric_sint64 ev)

instance Metricable Integer where
  metric lift ev =
    fmap (\v -> ev {E.metric_sint64 = fmap fromInteger v})
    $ lift (fmap fromIntegral $ E.metric_sint64 ev)

instance Metricable Double where
  metric lift ev =
    fmap (\v -> ev {E.metric_d = v}) $ lift (E.metric_d ev)

instance Metricable Float where
  metric lift ev =
    fmap (\v -> ev {E.metric_f = v}) $ lift (E.metric_f ev)