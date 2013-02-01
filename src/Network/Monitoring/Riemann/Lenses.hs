{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.Monitoring.Riemann.Lenses (
  Stated (..),
  Metricable (..),
  Event,
  attributes,
  State,
  service, once,
  Msg,
  ok, merror, states, query, events,
  Query,
  qstring
  ) where

import Control.Lens
import Control.Applicative
import Data.Monoid
import Data.Foldable
import Data.Sequence as Sequence
import           Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
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

toutf8 :: Text -> Utf8
toutf8 = Utf8 . TE.encodeUtf8 

fromutf8 :: Utf8 -> Text
fromutf8 (Utf8 lbs) = TE.decodeUtf8 lbs

class Stated a where
  time        :: Simple Lens a (Maybe Int64)
  state       :: Simple Lens a (Maybe Text)
  host        :: Simple Lens a (Maybe Text)
  description :: Simple Lens a (Maybe Text)
  tags        :: Simple Lens a (Seq Text)
  ttl         :: Simple Lens a (Maybe Float)

class Metricable a where
  metric :: Simple Lens Event (Maybe a)

instance Stated Event where
  time f ev = fmap (\a -> ev {Ev.time = a}) $ f (Ev.time ev)

  state f ev =
    fmap (\a -> ev {Ev.state = fmap toutf8 a})
    $ f (fmap fromutf8 $ Ev.state ev)

  host f ev =
    fmap (\a -> ev {Ev.host = fmap toutf8 a})
    $ f (fmap fromutf8 $ Ev.host ev)

  description f ev =
    fmap (\a -> ev {Ev.description = fmap toutf8 a})
    $ f (fmap fromutf8 $ Ev.description ev)

  tags f ev =
    fmap (\a -> ev {Ev.tags = fmap toutf8 a})
    $ f (fmap fromutf8 $ Ev.tags ev)

  ttl f ev = fmap (\a -> ev {Ev.ttl = a}) $ f (Ev.ttl ev)

attributes :: Simple Lens Event (Map Text (Maybe Text))
attributes f ev = fmap (\a -> ev {Ev.attributes = enter a}) $ f (exit $ Ev.attributes ev)
  where exit :: Seq At.Attribute -> Map Text (Maybe Text)
        exit = foldMap (\attr -> M.singleton
                                 (fromutf8 $ At.key attr)
                                 (fmap fromutf8 $ At.value attr))
        enter :: Map Text (Maybe Text) -> Seq At.Attribute
        enter = M.foldrWithKey
                (\k a b -> b |> At.Attribute { At.key = toutf8 k,
                                               At.value = fmap toutf8 a })
                mempty

instance Metricable Int64 where
  metric f ev = fmap (\a -> ev {Ev.metric_sint64 = a}) $ f (Ev.metric_sint64 ev)

instance Metricable Double where
  metric f ev = fmap (\a -> ev {Ev.metric_d = a}) $ f (Ev.metric_d ev)

instance Metricable Float where
  metric f ev = fmap (\a -> ev {Ev.metric_f = a}) $ f (Ev.metric_f ev)

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
      (Ev.Event { Ev.time = t1 +++ t2,
                  Ev.state = s1 +++ s2,
                  Ev.service = comb " " se1 se2,
                  Ev.host = comb "." h1 h2,
                  Ev.description = d1 +++ d2,
                  Ev.tags = ta1 <> ta2,
                  Ev.ttl = liftA2 max tl1 tl2,
                  Ev.attributes = a1 <> a2,
                  Ev.metric_sint64 = msi1 +++ msi2,
                  Ev.metric_d = md1 +++ md2,
                  Ev.metric_f = mf1 +++ mf2 })
    where a +++ b = getLast $ Last a <> Last b
          comb x a b = liftA2
                       (\st1 st2 -> toutf8 $ fromutf8 st1 <> x <> fromutf8 st2)
                       a b
          

instance Stated State where
  time f ev = fmap (\a -> ev {St.time = a}) $ f (St.time ev)

  state f ev =
    fmap (\a -> ev {St.state = fmap toutf8 a})
    $ f (fmap fromutf8 $ St.state ev)

  host f ev =
    fmap (\a -> ev {St.host = fmap toutf8 a})
    $ f (fmap fromutf8 $ St.host ev)

  description f ev =
    fmap (\a -> ev {St.description = fmap toutf8 a})
    $ f (fmap fromutf8 $ St.description ev)

  tags f ev =
    fmap (\a -> ev {St.tags = fmap toutf8 a})
    $ f (fmap fromutf8 $ St.tags ev)

  ttl f ev = fmap (\a -> ev {St.ttl = a}) $ f (St.ttl ev)

service :: Simple Lens State (Maybe Text)
service f ev = fmap (\a -> ev {St.service = fmap toutf8 a})
               $ f (fmap fromutf8 $ St.service ev)

once :: Simple Lens State (Maybe Bool)
once f ev = fmap (\a -> ev {St.once = a}) $ f (St.once ev)

instance Monoid State where
  mempty = defaultValue
  mappend
    (St.State {St.time = t1,
               St.state = s1,
               St.service = se1,
               St.host = h1,
               St.description = d1,
               St.once = o1,
               St.tags = ta1,
               St.ttl = tl1})
    (St.State {St.time = t2,
               St.state = s2,
               St.service = se2,
               St.host = h2,
               St.description = d2,
               St.once = o2,
               St.tags = ta2,
               St.ttl = tl2}) =
      (St.State {St.time = t1 +++ t2,
                 St.state = s1 +++ s2,
                 St.service = comb " " se1 se2,
                 St.host = comb "." h1 h2,
                 St.description = d1 +++ d2,
                 St.once = o1 +++ o2,
                 St.tags = ta1 <> ta2,
                 St.ttl = liftA2 max tl1 tl2 })
    where a +++ b = getLast $ Last a <> Last b
          comb x a b = liftA2
                       (\st1 st2 -> toutf8 $ fromutf8 st1 <> x <> fromutf8 st2)
                       a b
          


ok :: Simple Lens Msg (Maybe Bool)
ok f ev =
  fmap (\a -> ev {Ms.ok = a}) $ f (Ms.ok ev)

merror :: Simple Lens Msg (Maybe Text)
merror f ev =
  fmap (\a -> ev {Ms.error = fmap toutf8 a})
  $ f (fmap fromutf8 $ Ms.error ev)

states :: Simple Lens Msg (Seq State)
states f ev =
  fmap (\a -> ev {Ms.states = a}) $ f (Ms.states ev)

query :: Simple Lens Msg (Maybe Query)
query f ev =
  fmap (\a -> ev {Ms.query = a})
  $ f (Ms.query ev)

events :: Simple Lens Msg (Seq Event)
events f ev =
  fmap (\a -> ev {Ms.events = a}) $ f (Ms.events ev)

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
      (Ms.Msg { Ms.ok = liftA2 (&&) o1 o2,
                Ms.error = e1 +++ e2,
                Ms.states = s1 <> s2,
                Ms.query = q1 +++ q2,
                Ms.events = ev1 <> ev2 })
    where a +++ b = getLast $ Last a <> Last b


qstring :: Simple Lens Query (Maybe Text)
qstring f ev = fmap (\a -> ev {Qu.string = fmap toutf8 a})
               $ f (fmap fromutf8 $ Qu.string ev)

instance Monoid Query where
  mempty = defaultValue
  mappend (Qu.Query s1) (Qu.Query s2) = Qu.Query { Qu.string = s1 +++ s2 }
    where a +++ b = getLast $ Last a <> Last b