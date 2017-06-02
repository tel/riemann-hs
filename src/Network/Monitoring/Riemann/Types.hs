{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeOperators     #-}

module Network.Monitoring.Riemann.Types (
  HasState (..),
  AMetric (..),
  HasQuery (..),
  State, Event, Query, Msg,
  ev,
  once, attributes, time_micros,
  MsgState(..), msgState, states, events,
  Hostname, Port
  ) where

import           Control.Arrow
import           Control.Lens
import           Control.Monad
import           Data.Default
import           Data.Int
import           Data.List
import           Data.Map             (Map)
import qualified Data.Map             as M
import           Data.Maybe
import           Data.ProtocolBuffers
import           Data.Text            (Text)
import qualified Data.Text            as T
import           GHC.Generics         hiding (from, to)
import qualified GHC.Generics         as G

-- $class

-- | 'HasState' types (e.g. 'Event' and 'State') have information
-- representing the state of a 'service' on a 'host' at a given
-- 'time'. These shared types give rise to restrictedly polymorphic
-- lenses.
class HasState a where
  time        :: Lens' a (Maybe Int64)
  -- ^ The time of the event, in unix epoch seconds
  state       :: Lens' a (Maybe Text)
  -- ^ Any string less than 255 bytes, e.g. "ok", "warning",
  -- "critical"
  service     :: Lens' a (Maybe Text)
  -- ^ e.g. "API port 8000 reqs/sec"
  host        :: Lens' a (Maybe Text)
  -- ^ A hostname, e.g. "api1", "foo.com"
  description :: Lens' a (Maybe Text)
  -- ^ Freeform text
  tags        :: Lens' a [Text]
  -- ^ Freeform list of strings, e.g. ["rate", "fooproduct",
  -- "transient"]
  ttl         :: Lens' a (Maybe Float)
  -- ^ A floating-point time, in seconds, that this event is
  -- considered valid for. Expired states may be removed from the
  -- index.

-- | 'HasQuery' types contain a Riemann query inside them
-- somewhere. This class provides 'query' as a polymorphic lens toward
-- that query.
class HasQuery a where
  query :: Lens' a (Maybe Text)

-- $generics

class GMonoid f where
    gmempty :: f a
    gmappend :: f a -> f a -> f a

instance GMonoid U1 where
    gmempty = U1
    gmappend U1 U1 = U1

instance (GMonoid a, GMonoid b) => GMonoid (a :*: b) where
    gmempty = gmempty :*: gmempty
    gmappend (a :*: x) (b :*: y) = gmappend a b :*: gmappend x y

instance Monoid a => GMonoid (K1 i a) where
    gmempty = K1 mempty
    gmappend (K1 x) (K1 y) = K1 $ mappend x y

instance GMonoid a => GMonoid (M1 i c a) where
    gmempty = M1 gmempty
    gmappend (M1 x) (M1 y) = M1 $ gmappend x y

defMappend :: (Generic a, GMonoid (Rep a)) => a -> a -> a
defMappend x y = G.to $ G.from x `gmappend` G.from y

-- $types

-- | 'State' is an object within Riemann's index, a result from a
-- 'Query'.
data State = State {
  _stateTime        :: Optional 1 (Value Int64),
  _stateState       :: Optional 2 (Value Text),
  _stateService     :: Optional 3 (Value Text),
  _stateHost        :: Optional 4 (Value Text),
  _stateDescription :: Optional 5 (Value Text),
  _stateOnce        :: Optional 6 (Value Bool),
  _stateTags        :: Repeated 7 (Value Text),
  _stateTtl         :: Optional 8 (Value Float)
  } deriving (Eq, Generic)

-- | 'Event' is a description of an application-level event, emitted
-- to Riemann for indexing.
data Event = Event {
  _eventTime        :: Optional 1 (Value Int64),
  _eventState       :: Optional 2 (Value Text),
  _eventService     :: Optional 3 (Value Text),
  _eventHost        :: Optional 4 (Value Text),
  _eventDescription :: Optional 5 (Value Text),
  _eventTags        :: Repeated 7 (Value Text),
  _eventTtl         :: Optional 8 (Value Float),

  _eventAttributes  :: Repeated 9 (Message Attribute),
  _eventTimeMicros  :: Optional 10 (Value Int64),
  _eventMetricSInt  :: Optional 13 (Value (Signed Int64)),
  _eventMetricD     :: Optional 14 (Value Double),
  _eventMetricF     :: Optional 15 (Value Float)
  } deriving (Eq, Generic)

-- | 'Query' is a question to be made of the Riemann index.
data Query = Query { _queryQuery :: Optional 1 (Value Text) }
           deriving (Eq, Generic)

-- | 'Msg' is a wrapper for sending/receiving multiple 'State's,
-- 'Event's, or a single 'Query'.
data Msg = Msg {
  _msgOk     :: Optional 2 (Value Bool),
  _msgError  :: Optional 3 (Value Text),
  _msgStates :: Repeated 4 (Message State),
  _msgQuery  :: Optional 5 (Message Query),
  _msgEvents :: Repeated 6 (Message Event)
  } deriving (Eq, Generic)

-- | 'Attribute' is a key/value pair.
data Attribute = Attribute {
  _attributeKey   :: Required 1 (Value Text),
  _attributeValue :: Optional 2 (Value Text)
  } deriving (Eq, Show, Generic)

-- $state

instance Encode State
instance Decode State
$(makeLenses ''State)

instance HasState State where
  time        = stateTime . field
  state       = stateState . field
  service     = stateService . field
  host        = stateHost . field
  description = stateDescription . field
  tags        = stateTags . field
  ttl         = stateTtl . field

once :: Lens' State (Maybe Bool)
once = stateOnce . field

instance Show State where
  show s = "State { " ++ intercalate ", " innards ++ " }"
    where innards = catMaybes [
            showM "time" time,
            showM "state" state,
            showM "service" service,
            showM "host" host,
            showM "description" description,
            showL "tags" tags,
            showM "ttl" ttl,
            showM "once" once
            ]
          showM name l = (\x -> name ++ " = " ++ x) . show <$> s ^. l
          showL name l = let lst = s ^. l
                         in if null lst then Nothing else Just $ name ++ " = " ++ show lst

instance Default State where
  def = State {
    _stateTime        = putField Nothing,
    _stateState       = putField Nothing,
    _stateService     = putField Nothing,
    _stateHost        = putField Nothing,
    _stateDescription = putField Nothing,
    _stateTags        = putField [],
    _stateTtl         = putField Nothing,
    _stateOnce        = putField Nothing
    }

instance Monoid State where
  mempty = def
  mappend = defMappend

-- $attribute

instance Encode Attribute
instance Decode Attribute
$(makeLenses ''Attribute)

akey :: Lens' Attribute Text
akey = attributeKey . field

aval :: Lens' Attribute (Maybe Text)
aval = attributeValue . field

apair :: Iso' Attribute (Text, Maybe Text)
apair = iso (view akey &&& view aval)
            (\(k, v) -> Attribute (putField k) (putField v))

-- $event

instance Encode Event
instance Decode Event
$(makeLenses ''Event)

instance HasState Event where
  time        = eventTime . field
  state       = eventState . field
  service     = eventService . field
  host        = eventHost . field
  description = eventDescription . field
  tags        = eventTags . field
  ttl         = eventTtl . field

-- | The class of types which can be interpreted as metrics for an
-- 'Event'.
class AMetric a where
  metric :: Lens' Event (Maybe a)

instance AMetric Int where
  metric = eventMetricSInt . field . mapping (iso fromIntegral fromIntegral)
instance AMetric Integer where
  metric = eventMetricSInt . field . mapping (iso fromIntegral fromIntegral)
instance AMetric (Signed Int64) where
  metric = eventMetricSInt . field

instance AMetric Double where metric = eventMetricD    . field
instance AMetric Float  where metric = eventMetricF    . field

attributes :: Lens' Event (Map Text Text)
attributes = eventAttributes
             . field
             . mapping apair
             -- This isn't really an iso, it throws away `(_, Nothing)`s
             -- but I'm okay with that since these just represent
             -- "empty" attributes.
             . iso (mapMaybe sequen) (map $ over _2 Just)
             . iso M.fromList M.toList
  where sequen :: Applicative f => (a, f b) -> f (a, b)
        sequen (a, fb) = (a,) <$> fb

-- | optional precision for the event time
time_micros :: Lens' Event (Maybe Int64)
time_micros = eventTimeMicros . field

instance Show Event where
  show s = "Event { " ++ intercalate ", " innards ++ " }"
    where innards = catMaybes [
            showM "time" time,
            showM "time_micros" time_micros,
            showM "state" state,
            showM "service" service,
            showM "host" host,
            showM "description" description,
            showL "tags" tags,
            showM "ttl" ttl,
            showMap "attributes" attributes,
            showM "metric_sint" (metric :: Lens' Event (Maybe Int)),
            showM "metric_f" (metric :: Lens' Event (Maybe Float)),
            showM "metric_d" (metric :: Lens' Event (Maybe Double))
            ]
          showM name l = (\x -> name ++ " = " ++ x) . show <$> s ^. l
          showMap name l = let mp = s ^. l
                           in if M.null mp then Nothing
                              else Just . (\x -> name ++ " = " ++ show x) $ mp
          showL name l = let lst = s ^. l
                         in if null lst then Nothing
                            else Just $ name ++ " = " ++ show lst

instance Default Event where
  def = Event {
    _eventTime        = putField Nothing,
    _eventState       = putField Nothing,
    _eventService     = putField Nothing,
    _eventHost        = putField Nothing,
    _eventDescription = putField Nothing,
    _eventTags        = putField [],
    _eventTtl         = putField Nothing,
    _eventTimeMicros  = putField Nothing,
    _eventAttributes  = putField [],
    _eventMetricSInt  = putField Nothing,
    _eventMetricD     = putField Nothing,
    _eventMetricF     = putField Nothing
    }

instance Monoid Event where
  mempty = def
  mappend = defMappend

-- Nicer constructors

-- | Create a simple 'Event' with state "ok".
--
-- >>> view state $ ev "service" (0 :: (Signed Int64))
-- Just "ok"
--
-- >>> view service $ ev "service" (0 :: (Signed Int64))
-- Just "service"
--
-- >>> view metric $ ev "service" (0 :: (Signed Int64)) :: Maybe (Signed Int64)
-- Just (Signed 0)
--
-- >>> view tags $ ev "service" (0 :: (Signed Int64))
-- []
ev :: AMetric a => String -> a -> Event
ev serv met =
  def
  & state ?~ "ok"
  & service ?~ T.pack serv
  & metric ?~ met

-- $query

instance Encode Query
instance Decode Query
$(makeLenses ''Query)

instance HasQuery Query where
  query = queryQuery . field

instance Default Query where
  def = Query { _queryQuery = putField Nothing }

instance Monoid Query where
  mempty = def
  mappend = defMappend

instance Show Query where
  show s = "Query { " ++ intercalate ", " innards ++ " }"
    where innards = catMaybes [
            showM "query" query
            ]
          showM name l = (\x -> name ++ " = " ++ x) . show <$> s ^. l

-- $msg

data MsgState = Ok | Error Text | Unknown

instance Encode Msg
instance Decode Msg
$(makeLenses ''Msg)

msgState :: Lens' Msg MsgState
msgState = iso dup fst
           . alongside (msgOk . field) (msgError . field)
           . iso toMsgState fromMsgState
  where dup x = (x, x)
        toMsgState (_,          Just err) = Error err
        toMsgState (Just True , Nothing ) = Ok
        toMsgState (Just False, Nothing ) = Error "<no msg>"
        toMsgState (Nothing   , Nothing ) = Unknown
        fromMsgState Ok = (Just True, Nothing)
        fromMsgState (Error err) = (Just False, Just err)
        fromMsgState Unknown = (Nothing, Nothing)

states :: Lens' Msg [State]
states = msgStates . field

events :: Lens' Msg [Event]
events = msgEvents . field

instance Show Msg where
  show s = "Msg { " ++ intercalate ", " innards ++ " }"
    where innards = catMaybes [
            showMsgState,
            showL "states" states,
            showL "events" events,
            showM "query" query
            ]
          showM name l = (\x -> name ++ " = " ++ x) . show <$> s ^. l
          showL name l = let lst = s ^. l
                         in if null lst then Nothing else Just $ name ++ " = " ++ show lst
          showMsgState = ("msgState = " ++) <$> case s ^. msgState of
            Ok -> Just "Ok"
            Error err -> Just $ "Error " ++ show err
            Unknown -> Nothing

instance HasQuery Msg where
  query = msgQuery . field
          . mapping (iso (getField . _queryQuery) (Query . putField))
          . iso join return

instance Default Msg where
  def = Msg {
    _msgOk = putField Nothing,
    _msgError = putField Nothing,
    _msgStates = putField [],
    _msgQuery = putField Nothing,
    _msgEvents = putField []
    }

instance Monoid Msg where
  mempty = def
  mappend = defMappend


type Hostname = String
type Port     = Int
