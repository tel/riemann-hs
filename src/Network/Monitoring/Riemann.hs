{-# LANGUAGE OverloadedStrings #-}

module Network.Monitoring.Riemann (
  module Network.Monitoring.Riemann.Types,
  module Data.Int,
  Client,
  makeClient,
  closeClient,
  sendEvent',
  sendEvent
  ) where

import Network.Monitoring.Riemann.Types

import Data.Int
import Data.Default
import Data.Time.Clock.POSIX
import Data.ProtocolBuffers
import Data.Serialize.Put

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Error
import Control.Lens
import Control.Exception

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

{-%

In brief, a Riemann client has two operating conditions: (a) as a
decoration of *real* code or (b) as a component of self-reflecting
system component. In 95% of cases the client will be used for (a), so
that's the easiest way to use the client.

An (a)-style Riemann client should allow for liberal *decoration* of
code with monitoring keys. These decorations should trivially reduce
to nops if there is no connection to a Riemann server and they should
silently ignore all server errors. The (a)-style Riemann decorations
should never slow real code and thus must either be very, very fast or
asynchronous.

As a tradeoff, we can never be sure that *all* (a)-style decorations
fire and are observed by a Riemann server. Neither the client or the
server can take note of or be affected by packet failure.

A (b)-style Riemann client should allow for smart load balancing. It
should be able to guarantee connectivity to the Riemann server and
failover along with the server should Riemann ever die or become
partitioned. (To this end, there's some need for pools of Riemann
servers, but this may be non-critical.) Riemann (b)-style interactions
also include querying the Riemann server --- so we'll need a query
combinator language.

-}

{-%

API Design
----------

Basic events ought to be generated very easily. Sane defaults ought to
be built-in---we shouldn't be specifying the host in every decorated
call, we shouldn't have any concept of the current time when we
decorate an action. To this end the Monoid instances for `Event`s,
`State`s, `Msg`s, and `Query`s are designed to either grow or be
overridden to the right (using lots of `Last` newtypes over maybes and
inner `(<>)` applications).

The Client also should be defaulted at as high a level as possible.

e.g.

```
withClient :: Client -> IO a -> IO a
withDefaultEvent :: Event -> IO a -> IO a
withEventTiming :: IO a -> IO a
withHostname :: Text -> IO a -> IO a
```

-}

{-%

Implementation
--------------

There are roughly two independent factors for library design. First,
we can use UDP or TCP---Riemann size limits UDP datagrams, but the
limit is high (16 mb by default), so there's theoretically a corner
case there but it's a fair bet that we won't hit it---and secondly we
can deliver them in the main thread or asynchronously via a concurrent
process.

There's a tradeoff here between throughput and assurance. Asynch+UDP
has the highest throughput, while Synch+TCP has the greatest
assurance. We'll optimize for (a)-type decoration via Asynch+UDP.

Can we do the same and optimize (b)-type calls as Synch+TCP? Probably.

-}

{-%

Syntax
------

riemann $ ev "<service>" <metric> & tags <>~ "foo"

-}

data Client = UDP { unClient :: Either SomeException (Socket, AddrInfo) }
            deriving (Show)

type Hostname = String
type Port     = Int

-- | Attempts to bind a UDP client at the passed 'Hostname' and
-- 'Port'. Failures are silently ignored---failure in monitoring
-- should not cause an application failure...
makeClient :: Hostname -> Port -> IO Client
makeClient hn po = UDP <$> sock
  where sock :: IO (Either SomeException (Socket, AddrInfo))
        sock =
          try $ do addrs <- getAddrInfo
                            (Just $ defaultHints {
                                addrFlags = [AI_NUMERICSERV] })
                            (Just hn)
                            (Just $ show po)
                   case addrs of
                     []       -> fail "No accessible addresses"
                     (addy:_) -> do
                       s <- socket (addrFamily addy)
                                   Datagram
                                   defaultProtocol -- (addrProtocol addy)
                       return (s, addy)

closeClient :: Client -> IO ()
closeClient c = either (const $ return ()) (close . fst) $ unClient c

-- | Attempts to forward an event to a client. Fails silently.
sendEvent :: MonadIO m => Client -> Event -> m ()
sendEvent c = liftIO . void . runEitherT . sendEvent' c

-- | Attempts to forward an event to a client. If it fails, it'll
-- return an 'IOException' in the 'Either'.
sendEvent' :: Client -> Event -> EitherT IOException IO ()
sendEvent' (UDP (Left e))  _ = liftIO (print e) >> return ()
sendEvent' (UDP (Right (s, addy))) e = tryIO $ do
  now <- fmap round getPOSIXTime
  let msg = def & events .~ [e & time ?~ now]
  void $ sendTo s (runPut $ encodeMessage msg) (addrAddress addy)

