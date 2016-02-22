{-# LANGUAGE RecordWildCards #-}
-- | TCP Specific connection handling for monitoring with Riemann
module Network.Monitoring.Riemann.TCP (
  module Network.Monitoring.Riemann.Types,
  TCPClient, makeTCPClientFromConnection, makeTLSClient, makeTCPClient,
  sendEventTCP, sendQueryTCP, sendEventTCP', sendQueryTCP'
  ) where

import           Control.Concurrent.Async         (waitCatch, withAsync)
import           Control.Concurrent.MVar
import           Control.Exception.Base           (SomeException, bracket)
import           Control.Lens                     ((&), (.~), (?~), (^.))
import qualified Data.ByteString                  as BS
import           Data.Default                     (def)
import           Data.Monoid
import           Data.ProtocolBuffers
import           Data.Serialize.Get
import           Data.Serialize.Put
import           Data.Text
import           Data.Time.Clock                  (getCurrentTime)
import           Data.Time.Clock.POSIX            (utcTimeToPOSIXSeconds)
import           Network.Connection
import           Network.Monitoring.Riemann.Types

type Hostname = String
type Port     = Int

-- | An opaque data type for a TCP Riemann client. It currently does not support
-- connection pooling; concurrent uses will be blocked.
data TCPClient = TCPClient
  { conn :: Connection
  , lock :: MVar () }

-- | Perform an action and catch all synchronous exceptions.
tryAny :: IO a -> IO (Either SomeException a)
tryAny action = withAsync action waitCatch

-- | Perform an action while holding a lock (binary semaphore).
takeLockDo :: MVar () -> IO a -> IO a
takeLockDo lock action = bracket (takeMVar lock) (\_ -> putMVar lock ()) (const action)

-- | Create a @TCPClient@ from an existing @Connection@.
makeTCPClientFromConnection :: Connection -> IO TCPClient
makeTCPClientFromConnection conn = TCPClient conn <$> newMVar ()

-- | Create a @TCPClient@ from host name, port, and @TLSSettings@. The
-- connection will use TLS.
makeTLSClient :: Hostname -> Port -> TLSSettings -> IO TCPClient
makeTLSClient hn po tls = do
  ctx <- initConnectionContext
  let params = ConnectionParams hn (fromIntegral po) (Just tls) Nothing
  TCPClient <$> connectTo ctx params <*> newMVar ()

-- | Create a @TCPClient@ from host name and port. The connection will not use
-- TLS.
makeTCPClient :: Hostname -> Port -> IO TCPClient
makeTCPClient hn po = do
  ctx <- initConnectionContext
  let params = ConnectionParams hn (fromIntegral po) Nothing Nothing
  TCPClient <$> connectTo ctx params <*> newMVar ()

-- | Send a message.
sendMsg :: TCPClient -> Msg -> IO ()
sendMsg TCPClient{..} msg = do
  let bytes = runPut (encodeMessage msg)
      bytesWithLen = runPut (putWord32be (fromIntegral $ BS.length bytes)  >> putByteString bytes)
  connectionPut conn bytesWithLen

-- | Receive a message.
receiveMsg :: TCPClient -> IO Msg
receiveMsg TCPClient{..} = do
  msgLenBS <- readExactlyNBytes 4
  let msgLen = fromIntegral $ runGet' getWord32be msgLenBS
  msgBs <- readExactlyNBytes msgLen
  return $ runGet' decodeMessage msgBs

  where readExactlyNBytes n = do
          bs <- connectionGet conn n
          if BS.length bs == n then return bs else do
            nextPart <- readExactlyNBytes (n - BS.length bs)
            return (bs <> nextPart)
        runGet' g bs = either (error . ("cannot deserialise: " <>)) id $ runGet g bs

-- | Send an event with the @TCPClient@. If it fails, it will return the
-- exception in a @Left@ value.
sendEventTCP' :: TCPClient -> Event -> IO (Either SomeException ())
sendEventTCP' conn e = tryAny $ do
  current <- getCurrentTime
  let now = round (utcTimeToPOSIXSeconds current)
  let msg = def & events .~ [e & time ?~ now]
  takeLockDo (lock conn) $ sendMsg conn msg

-- | Send a query with the @TCPClient@ and wait for a reply. If it fails, it
-- will return the exception in a @Left@ value.
sendQueryTCP' :: TCPClient -> Text -> IO (Either SomeException ([State], [Event]))
sendQueryTCP' conn q = tryAny $ do
  let msg = def & query .~ Just q
  rcvd <- takeLockDo (lock conn) $ do
    sendMsg conn msg
    receiveMsg conn
  return (rcvd ^. states, rcvd ^. events)

-- | Send an event with the @TCPClient@. If it fails, it will silently discard
-- the exception.
sendEventTCP :: TCPClient -> Event -> IO ()
sendEventTCP conn e = either (const def) id <$> sendEventTCP' conn e

-- | Send a query with the @TCPClient@ and wait for a reply. If it fails, a
-- default (i.e. empty) value will be returned.
sendQueryTCP :: TCPClient -> Text -> IO ([State], [Event])
sendQueryTCP conn q = either (const def) id <$> sendQueryTCP' conn q
