{-# LANGUAGE OverloadedStrings #-}

module Network.Monitoring.Riemann (
  module Network.Monitoring.Riemann.Lenses
  ) where

import Network.Monitoring.Riemann.Lenses

import Data.Monoid
import Data.Text.Lazy (Text)
import qualified Data.ByteString.Lazy as L
import Control.Applicative
import Control.Monad
import Control.Error
import Control.Exception
import Control.Lens

import Text.ProtocolBuffers
import Network.Socket
import qualified Network.Socket.ByteString as NBS

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

-- What kinds of monitors do we want?

-- A default client should be specified which overwhelms all of these

-- TCP clients should be MChan based independent TCP clients
-- UDP clients can just be pure send/receive

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

data Client = UDP (Maybe (Socket, AddrInfo))
            deriving (Show, Eq)

type Hostname = String
type Port     = Int

-- | Attempts to bind a UDP client at the passed 'Hostname' and
-- 'Port'. Failures are silently ignored---failure in monitoring
-- should not cause an application failure...
makeClient :: Hostname -> Port -> IO Client
makeClient hn po = UDP . rightMay <$> sock
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
                                   (addrProtocol addy)
                       return (s, addy)

ev :: Metricable a => Text -> Text -> a -> Event
ev h s m = (host .~ Just h)
           . (state .~ Just "ok")
           . (service .~ Just s)
           . (metric  .~ Just m)
           $ mempty

-- | Attempts to forward an event to a client. Fails silently.
sendEvent :: Client -> Event -> IO ()
sendEvent c = void . runEitherT . sendEvent' c

-- | Attempts to forward an event to a client. If it fails, it'll
-- return an 'IOException' in the 'Either'.
sendEvent' :: Client -> Event -> EitherT IOException IO ()
sendEvent' (UDP Nothing)  _ = return ()
sendEvent' (UDP (Just (s, addy))) e =
  tryIO
  $ (\bs -> NBS.sendManyTo s bs (addrAddress addy))
  $ L.toChunks $ runPut $ messagePutM
  $ (events .~ [e]) mempty