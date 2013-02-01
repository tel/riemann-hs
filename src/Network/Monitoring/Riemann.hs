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

import System.Random

-- What kinds of monitors do we want?

-- A default client should be specified which overwhelms all of these

-- TCP clients should be MChan based independent TCP clients
-- UDP clients can just be pure send/receive

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

spam = do c <- makeClient "localhost" 5555
          sequence_ $ replicate 3000 (m c)
  where m c = do v <- randomRIO (0, 500) :: IO Float
                 sendEvent c (ev "localhost" "value" v)