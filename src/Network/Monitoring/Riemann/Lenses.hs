{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

module Network.Monitoring.Riemann.Lenses (
  (..~),
  Stated (..), Int64,
  Metricable (..),
  Event,
  attributes, attributes', attributeMap,
  State,
  once,
  Msg,
  ok, merror, states, states', query, events, events',
  Query,
  qstring
  ) where

import GHC.Int (Int64)

import Control.Lens
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

utfI :: Simple Iso Utf8 Text
utfI = iso (\(Utf8 lbs) -> TE.decodeUtf8 lbs) (Utf8 . TE.encodeUtf8)

-- | A slight variation on '(.~)' which also includes applying 'pure'
-- over the 'Setting' Read it as "set pure". It's very useful for
-- setting properties on 'Event's and 'State's without littering
-- 'Just's everywhere.
infixr 4 ..~
(..~) :: Applicative f => Setting s t a (f b) -> b -> s -> t
l ..~ v = l .~ pure v

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
  mappend
    (Ev.Event { Ev.time = t1,
                Ev.state = s1,
                Ev.service = se1,
                Ev.host = h1,
                Ev.description = d1,
                Ev.tags = ta1,
                Ev.ttl = tl1,
                Ev.attributes = a1,
                Ev.metric_sint64 = msi1,
                Ev.metric_d = md1,
                Ev.metric_f = mf1 })
    (Ev.Event { Ev.time = t2,
                Ev.state = s2,
                Ev.service = se2,
                Ev.host = h2,
                Ev.description = d2,
                Ev.tags = ta2,
                Ev.ttl = tl2,
                Ev.attributes = a2,
                Ev.metric_sint64 = msi2,
                Ev.metric_d = md2,
                Ev.metric_f = mf2 }) =
      Ev.Event { Ev.time = t1 +++ t2,
                 Ev.state = s1 +++ s2,
                 Ev.service = comb " " se1 se2,
                 Ev.host = comb "." h1 h2,
                 Ev.description = d1 +++ d2,
                 Ev.tags = ta1 <> ta2,
                 Ev.ttl = liftA2 max tl1 tl2,
                 Ev.attributes = a1 <> a2,
                 Ev.metric_sint64 = msi1 +++ msi2,
                 Ev.metric_d = md1 +++ md2,
                 Ev.metric_f = mf1 +++ mf2 }
    where a +++ b = getLast $ Last a <> Last b
          comb x = liftA2
                   (\st1 st2 -> review utfI $ view utfI st1 <> x <> view utfI st2)

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
  mappend
    (St.State { St.time = t1,
                St.state = s1,
                St.service = se1,
                St.host = h1,
                St.description = d1,
                St.once = o1,
                St.tags = ta1,
                St.ttl = tl1 })
    (St.State { St.time = t2,
                St.state = s2,
                St.service = se2,
                St.host = h2,
                St.description = d2,
                St.once = o2,
                St.tags = ta2,
                St.ttl = tl2 }) =
      St.State { St.time = t1 +++ t2,
                 St.state = s1 +++ s2,
                 St.service = comb " " se1 se2,
                 St.host = comb "." h1 h2,
                 St.description = d1 +++ d2,
                 St.once = o1 +++ o2,
                 St.tags = ta1 <> ta2,
                 St.ttl = liftA2 max tl1 tl2 }
    where a +++ b = getLast $ Last a <> Last b
          comb x = liftA2
                   (\st1 st2 -> review utfI $ view utfI st1 <> x <> view utfI st2)
  

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
  mappend
    (Ms.Msg { Ms.ok = o1,
              Ms.error = e1,
              Ms.states = s1,
              Ms.query = q1,
              Ms.events = ev1 })
    (Ms.Msg { Ms.ok = o2,
              Ms.error = e2,
              Ms.states = s2,
              Ms.query = q2,
              Ms.events = ev2 }) =
      Ms.Msg { Ms.ok = liftA2 (&&) o1 o2,
               Ms.error = e1 +++ e2,
               Ms.states = s1 <> s2,
               Ms.query = q1 +++ q2,
               Ms.events = ev1 <> ev2 }
    where a +++ b = getLast $ Last a <> Last b


-- $queries

$(makeLensesFor [("string", "qString")] ''Query)

qstring :: Simple Lens Query (Maybe Text)
qstring = qString . mapping utfI

instance Monoid Query where
  mempty = defaultValue
  mappend (Qu.Query s1) (Qu.Query s2) = Qu.Query { Qu.string = s1 +++ s2 }
    where a +++ b = getLast $ Last a <> Last b