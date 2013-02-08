{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-%

Interacting with big records like 'Event' and 'State' is challenging
and requires tracking two different function namespaces for the
getters and setters. This module rewraps all of those as lenses
distributed across a few type classes to make interacting with
'metric' and the overlap between 'Event' and 'State' easier.

This provides a much cleaner interface to the underlying Riemann types
costing only some clarity in types.

I also provide 'Monoid' instances for times when combining 'Event's or
'State's or 'Query's makes the most sense.

-}

module Network.Monitoring.Riemann.Types (
  Stated (..), Int64,
  Metricable (..),
  Event,
  attributes, attributes', attributeMap,
  evOk,
  State,
  once,
  Msg,
  ok, merror, states, states', query, events, events',
  Query,
  qstring
  ) where

import GHC.Int (Int64)

import Control.Lens hiding (elements)
import qualified Control.Monad.State as St
import Control.Applicative
import Control.Arrow
import Control.Error
import Data.Monoid
import Data.Maybe
import Data.Foldable
import qualified Data.Sequence as Sequence
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as TE

import Text.ProtocolBuffers
import Network.Monitoring.Riemann.Proto.Event (Event)
import Network.Monitoring.Riemann.Proto.State (State)
import Network.Monitoring.Riemann.Proto.Msg (Msg)
import Network.Monitoring.Riemann.Proto.Query (Query)
import qualified Network.Monitoring.Riemann.Proto.Event as Ev
import qualified Network.Monitoring.Riemann.Proto.State as St
import qualified Network.Monitoring.Riemann.Proto.Attribute as At
import qualified Network.Monitoring.Riemann.Proto.Msg as Ms
import qualified Network.Monitoring.Riemann.Proto.Query as Qu

import Test.QuickCheck hiding (once)

utfI :: Simple Iso Utf8 Text
utfI = iso (\(Utf8 lbs) -> TE.decodeUtf8 lbs) (Utf8 . TE.encodeUtf8)

-- $events

-- | 'Stated' types are 'Event' and 'State' which both have
-- information representing the state of a 'service' on a 'host' at a
-- given 'time'. These shared types give rise to restrictedly
-- polymorphic lenses.
class Stated a where
  time        :: Simple Lens a (Maybe Int64)
  state       :: Simple Lens a (Maybe Text)
  service     :: Simple Lens a (Maybe Text)
  host        :: Simple Lens a (Maybe Text)
  description :: Simple Lens a (Maybe Text)
  tags'       :: Simple Lens a (Seq Text)
  ttl         :: Simple Lens a (Maybe Float)

  tags         :: Simple Lens a [Text]
  tags = tags' . iso toList Sequence.fromList

-- | 'Metricable' types are those which can be 'metric's in an
-- 'Event'. This class provides dispatch from Haskell's polymorphic
-- types to Protobuf's non-polymorphic types.
class Metricable a where
  metric :: Simple Lens Event (Maybe a)

{-%

We automatically generate the core lenses, some of which will be
lifted into an instance of the `Stated` instance.

-}

$(makeLensesFor [("time", "evTime"),
                 ("state", "evState"),
                 ("service", "evService"),
                 ("host", "evHost"),
                 ("description", "evDescription"),
                 ("tags", "evTags"),
                 ("ttl", "evTtl"),
                 ("attributes", "evAttributes"),
                 ("metric_sint64", "evMetric_sint64"),
                 ("metric_d", "evMetric_d"),
                 ("metric_f", "evMetric_f")]
  ''Event)

instance Stated Event where
  time = evTime
  state = evState . mapping utfI
  service = evService . mapping utfI
  host = evHost . mapping utfI
  description = evDescription . mapping utfI
  tags' = evTags . mapping utfI
  ttl = evTtl

-- | Lensing 'At.Attributes'
attIso :: Simple Iso At.Attribute (Text, Maybe Text)
attIso = iso unAtt reAtt
  where
    unAtt :: At.Attribute -> (Text, Maybe Text)
    unAtt (At.Attribute { At.key = k, At.value = v }) =
      (view utfI k, fmap (view utfI) v)
    reAtt :: (Text, Maybe Text) -> At.Attribute
    reAtt (k, v) = At.Attribute { At.key = review utfI k,
                                  At.value = fmap (review utfI) v }

-- | This is the core attributes lens which just extracts the raw
-- underlying 'Seq' representation. The important thing to note is
-- that Protobufs allow for "empty" attributes but the other
-- convenience lenses ('attributes' and 'attributeMap') do not.
attributes' :: Simple Lens Event (Seq (Text, Maybe Text))
attributes' = evAttributes . mapping attIso
  where z = 3

-- | A lens convenient for setting attributes.
attributes :: Simple Lens Event [(Text, Text)]
attributes = attributes' . iso (mapMaybe liftMay . toList)
                               (Sequence.fromList . map (second Just))
  where liftMay (a, b) = (,) <$> pure a <*> b

-- | A lens convenient for getting attributes.
attributeMap :: Simple Lens Event (Map Text Text)
attributeMap = attributes . iso M.fromList M.toList

instance Metricable Int64 where
  metric = evMetric_sint64

instance Metricable Double where
  metric = evMetric_d

instance Metricable Float where
  metric = evMetric_f

instance Monoid Event where
  mempty = defaultValue
  e1 `mappend` e2 = flip St.execState e1 $ do
    time            >< last
    state           >< last
    host            >< (comb ".")
    description     >< last
    tags            >< mappend
    ttl             >< (liftA2 max)
    attributes      >< mappend
    evMetric_sint64 >< last
    evMetric_f      >< last
    evMetric_d      >< last
    where
      -- Update e1 with values from e2 combining with `op`
      (><) (lens :: Simple Lens s a) op = lens %= (`op` (e2 ^. lens))
      -- Some default "combiners"
      last a b = getLast $ Last a <> Last b
      comb x = liftA2 (\st1 st2 -> st1 <> x <> st2)

wrapMaybe :: Gen a -> Gen (Maybe a)
wrapMaybe a = oneof [pure Nothing, Just <$> a]

arbText :: Gen Text
arbText = T.pack <$> listOf1 (choose ('1', 'Z'))

instance Arbitrary Event where
  arbitrary = do
    -- random components
    atime        <- arbitrary `suchThatMaybe` (>0)
    astate       <- wrapMaybe (elements ["ok", "warning", "error", "failure", "banana"])
    aservice     <- wrapMaybe (T.unwords <$> listOf1 arbText)
    ahost        <- wrapMaybe (T.unwords <$> listOf1 arbText)
    adescription <- wrapMaybe (T.unwords <$> listOf1 arbText)
    sometags     <- listOf arbText
    attl         <- arbitrary `suchThatMaybe` (>0)
    someattrs    <- listOf $ (,) <$> arbText <*> arbText
    ametric      <- wrapMaybe arbitrary
    -- build into a random event
    return $ flip St.execState mempty $ do
      time        .= atime
      state       .= astate
      service     .= aservice
      host        .= ahost
      description .= adescription
      tags        .= sometags
      ttl         .= attl
      attributes  .= someattrs
      metric      .= (ametric :: Maybe Float)

-- | Create a simple 'Event' with state "ok".
--
-- >>> evOk (T.pack "service") (0 :: Int64) ^. state
-- Just "ok"
--
-- >>> evOk (T.pack "service") (0 :: Int64) ^. service
-- Just "service"
--
-- >>> evOk (T.pack "service") (0 :: Int64) ^. metric :: Maybe Int64
-- Just 0
--
-- >>> evOk (T.pack "service") (0 :: Int64) ^. tags
-- []
evOk :: Metricable a => Text -> a -> Event
evOk serv met =
  flip St.execState mempty $ do
    state   ?= "ok"
    service ?= serv
    metric  ?= met

-- $states

$(makeLensesFor [("time", "stTime"),
                 ("state", "stState"),
                 ("service", "stService"),
                 ("host", "stHost"),
                 ("description", "stDescription"),
                 ("once", "stOnce"),
                 ("tags", "stTags"),
                 ("ttl", "stTtl")]
  ''State)

instance Stated State where
  time = stTime
  state = stState . mapping utfI
  service = stService . mapping utfI
  host = stHost . mapping utfI
  description = stDescription . mapping utfI
  tags' = stTags . mapping utfI
  ttl = stTtl

-- This could just be done during the lens derivation, but this is
-- more explicit.
once :: Simple Lens State (Maybe Bool)
once = stOnce

instance Monoid State where
  mempty = defaultValue
  s1 `mappend` s2 = flip St.execState s1 $ do
    time        >< last
    state       >< last
    service     >< (comb " ")
    host        >< (comb ".")
    description >< last
    where
      -- Update s1 with values from s2 combining with `op`
      (><) (lens :: Simple Lens s a) op = lens %= (`op` (s2 ^. lens))
      -- Some default "combiners"
      last a b = getLast $ Last a <> Last b 
      comb x = liftA2 (\st1 st2 -> st1 <> x <> st2)

instance Arbitrary State where
  arbitrary = do
  -- random components
  atime        <- arbitrary `suchThatMaybe` (>0)
  astate       <- wrapMaybe (elements ["ok", "warning", "error", "failure", "banana"])
  aservice     <- wrapMaybe (T.unwords <$> listOf1 arbText)
  ahost        <- wrapMaybe (T.unwords <$> listOf1 arbText)
  adescription <- wrapMaybe (T.unwords <$> listOf1 arbText)
  sometags     <- listOf arbText
  attl         <- arbitrary `suchThatMaybe` (>0)
  aonce        <- wrapMaybe arbitrary
  -- build into a random event
  return $ flip St.execState mempty $ do
    time        .= atime
    state       .= astate
    service     .= aservice
    host        .= ahost
    description .= adescription
    tags        .= sometags
    ttl         .= attl
    once        .= aonce

-- $msgs

$(makeLensesFor [("ok", "msgOk"),
                 ("error", "msgError"),
                 ("states", "msgStates"),
                 ("query", "msgQuery"),
                 ("events", "msgEvents")]
  ''Msg)

ok :: Simple Lens Msg (Maybe Bool)
ok = msgOk

merror :: Simple Lens Msg (Maybe Text)
merror = msgError . mapping utfI

states' :: Simple Lens Msg (Seq State)
states' = msgStates

states :: Simple Lens Msg [State]
states = states' . iso toList Sequence.fromList

query :: Simple Lens Msg (Maybe Query)
query = msgQuery

events' :: Simple Lens Msg (Seq Event)
events' = msgEvents

events :: Simple Lens Msg [Event]
events = events' . iso toList Sequence.fromList

instance Monoid Msg where
  mempty = defaultValue
  m1 `mappend` m2 = flip St.execState m1 $ do
    ok     >< (liftA2 (&&))
    merror >< last
    states >< mappend
    query  >< last
    events >< mappend
    where
      -- Update m1 with values from m2 combining with `op`
      (><) (lens :: Simple Lens s a) op = lens %= (`op` (m2 ^. lens))
      -- Some default "combiners"
      last a b = getLast $ Last a <> Last b

instance Arbitrary Msg where
  arbitrary = do
  -- random components
  anok       <- wrapMaybe arbitrary
  anerror    <- wrapMaybe (T.unwords <$> listOf1 arbText)
  somestates <- listOf arbitrary
  aquery     <- wrapMaybe arbitrary
  someevents <- listOf arbitrary
  -- build into a random event
  return $ flip St.execState mempty $ do
    ok     .= anok
    merror .= anerror
    states .= somestates
    query  .= aquery
    events .= someevents

-- $queries

$(makeLensesFor [("string", "qString")] ''Query)

qstring :: Simple Lens Query (Maybe Text)
qstring = qString . mapping utfI

instance Monoid Query where
  mempty = defaultValue
  mappend (Qu.Query s1) (Qu.Query s2) = Qu.Query { Qu.string = s1 +++ s2 }
    where a +++ b = getLast $ Last a <> Last b

instance Arbitrary Query where
  -- TODO: Make a real arbitrary instance...
  arbitrary = pure $ mempty & (qstring ?~ "")