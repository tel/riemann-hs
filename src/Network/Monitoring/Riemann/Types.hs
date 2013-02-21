{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Network.Monitoring.Riemann.Types (
  HasState (..),
  AMetric (..),
  HasQuery (..),
  State, Event, Query, Msg,
  ev,
  once, attributes,
  MsgState, msgState, states, events
  ) where

import Data.ProtocolBuffers

import GHC.Generics hiding (D1, to, from)
import qualified GHC.Generics as G

import Data.TypeLevel.Num
import Data.Int
import Data.Monoid
import Data.Maybe
import Data.Time.Clock
import Data.Default
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as M

import Control.Lens
import Control.Monad
import Control.Arrow
import Control.Applicative

-- $class

-- | 'HasState' types (e.g. 'Event' and 'State') have information
-- representing the state of a 'service' on a 'host' at a given
-- 'time'. These shared types give rise to restrictedly polymorphic
-- lenses.
class HasState a where
  time        :: Lens' a (Maybe (Signed Int64))
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
  _stateTime        :: Optional' D1 (Signed Int64),
  _stateState       :: Optional' D2 Text,
  _stateService     :: Optional' D3 Text,
  _stateHost        :: Optional' D4 Text,
  _stateDescription :: Optional' D5 Text,
  _stateOnce        :: Optional' D6 Bool,
  _stateTags        :: Repeated  D7 Text,
  _stateTtl         :: Optional' D8 Float
  } deriving (Eq, Show, Generic)

-- | 'Event' is a description of an application-level event, emitted
-- to Riemann for indexing.
data Event = Event {
  _eventTime        :: Optional' D1 (Signed Int64),
  _eventState       :: Optional' D2 Text,
  _eventService     :: Optional' D3 Text,
  _eventHost        :: Optional' D4 Text,
  _eventDescription :: Optional' D5 Text,
  _eventTags        :: Repeated  D7 Text,
  _eventTtl         :: Optional' D8 Float,
  
  _eventAttributes  :: Repeated D9 (Message Attribute),
  _eventMetricSInt  :: Optional' (D1 :* D3) (Signed Int64),
  _eventMetricD     :: Optional' (D1 :* D4) Double,
  _eventMetricF     :: Optional' (D1 :* D5) Float
  } deriving (Eq, Show, Generic)

-- | 'Query' is a question to be made of the Riemann index.
data Query = Query { _queryQuery :: Optional' D1 Text }
           deriving (Eq, Show, Generic)

-- | 'Msg' is a wrapper for sending/receiving multiple 'State's,
-- 'Event's, or a single 'Query'.
data Msg = Msg {
  _msgOk     :: Optional' D2 Bool,
  _msgError  :: Optional' D3 Text,
  _msgStates :: Repeated D4 (Message State),
  _msgQuery  :: Optional  D5 (Message Query),
  _msgEvents :: Repeated D6 (Message Event)
  } deriving (Eq, Show, Generic)

-- | 'Attribute' is a key/value pair.
data Attribute = Attribute {
  _attributeKey   :: Required' D1 Text,
  _attributeValue :: Optional' D2 Text
  } deriving (Eq, Show, Generic)

-- $state

instance Encode State
instance Decode State
$(makeLenses ''State)

instance HasState State where
  time        = stateTime . value'
  state       = stateState . value'
  service     = stateService . value'
  host        = stateHost . value'
  description = stateDescription . value'
  tags        = stateTags . value'
  ttl         = stateTtl . value'

once :: Lens' State (Maybe Bool)
once = stateOnce . value'

instance Default State where
  def = State {
    _stateTime        = putValue' Nothing,
    _stateState       = putValue' Nothing,
    _stateService     = putValue' Nothing,
    _stateHost        = putValue' Nothing,
    _stateDescription = putValue' Nothing,
    _stateTags        = putValue [],
    _stateTtl         = putValue' Nothing,
    _stateOnce        = putValue' Nothing
    }

instance Monoid State where
  mempty = def
  mappend = defMappend

-- $attribute

instance Encode Attribute
instance Decode Attribute
$(makeLenses ''Attribute)

akey :: Lens' Attribute Text
akey = attributeKey . value'

aval :: Lens' Attribute (Maybe Text)
aval = attributeValue . value'

apair :: Iso' Attribute (Text, Maybe Text)
apair = iso (view akey &&& view aval)
            (\(k, v) -> Attribute (putValue' k) (putValue' v))

-- $event

instance Encode Event
instance Decode Event
$(makeLenses ''Event)

instance HasState Event where
  time        = eventTime . value'
  state       = eventState . value'
  service     = eventService . value'
  host        = eventHost . value'
  description = eventDescription . value'
  tags        = eventTags . value'
  ttl         = eventTtl . value'

-- | The class of types which can be interpreted as metrics for an
-- 'Event'.
class AMetric a where
  metric :: Lens' Event (Maybe a)

instance AMetric (Signed Int64)  where metric = eventMetricSInt . value'
instance AMetric Double where metric = eventMetricD    . value'
instance AMetric Float  where metric = eventMetricF    . value'

attributes :: Lens' Event (Map Text Text)
attributes = eventAttributes
             . value'
             . iso (mapMaybe runMessage) (map $ Message . Just)
             . mapping apair
             -- This isn't really an iso, it throws away `(_, Nothing)`s
             -- but I'm okay with that since these just represent
             -- "empty" attributes.
             . iso (mapMaybe sequen) (map $ over _2 Just)
             . iso M.fromList M.toList
  where sequen :: Applicative f => (a, f b) -> f (a, b)
        sequen (a, fb) = (a,) <$> fb
        
instance Default Event where
  def = Event {
    _eventTime        = putValue' Nothing,
    _eventState       = putValue' Nothing,
    _eventService     = putValue' Nothing,
    _eventHost        = putValue' Nothing,
    _eventDescription = putValue' Nothing,
    _eventTags        = putValue [],
    _eventTtl         = putValue' Nothing,
    _eventAttributes  = putValue' [],
    _eventMetricSInt  = putValue' Nothing,
    _eventMetricD     = putValue' Nothing,
    _eventMetricF     = putValue' Nothing
    }

instance Monoid Event where
  mempty = def
  mappend = defMappend

-- Nicer constructors

-- | Create a simple 'Event' with state "ok".
--
-- >>> get state $ ev "service" (0 :: (Signed Int64))
-- Just "ok"
--
-- >>> get service $ ev "service" (0 :: (Signed Int64))
-- Just "service"
--
-- >>> get metric $ ev "service" (0 :: (Signed Int64)) :: Maybe (Signed Int64)
-- Just 0
--
-- >>> get tags $ ev "service" (0 :: (Signed Int64))
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
  query = queryQuery . value'

instance Default Query where
  def = Query { _queryQuery = putValue' Nothing }

instance Monoid Query where
  mempty = def
  mappend = defMappend

-- $msg

data MsgState = Ok | Error Text | Unknown

instance Encode Msg
instance Decode Msg
$(makeLenses ''Msg)

msgState :: Lens' Msg MsgState
msgState = iso dup fst
           . alongside (msgOk . value') (msgError . value')
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
states = msgStates . value'
         . iso (mapMaybe runMessage) (map $ Message . Just)

events :: Lens' Msg [Event]
events = msgEvents . value'
         . iso (mapMaybe runMessage) (map $ Message . Just)

instance HasQuery Msg where
  query = msgQuery
          . message
          . mapping (iso (getValue' . _queryQuery) (Query . putValue'))
          . iso join return

instance Default Msg where
  def = Msg {
    _msgOk = putValue' Nothing,
    _msgError = putValue' Nothing,
    _msgStates = putValue' [],
    _msgQuery = putMessage Nothing,
    _msgEvents = putValue' []
    }

instance Monoid Msg where
  mempty = def
  mappend = defMappend
