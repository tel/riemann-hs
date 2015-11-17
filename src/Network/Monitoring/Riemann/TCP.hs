-- | TCP Specific connection handling for monitoring with Riemann
module Network.Monitoring.Riemann.TCP(tcpConnect, isTCPError, TCPState(..), doSendTCPEvent) where

import           Network.Monitoring.Riemann.Types

import qualified Data.ByteString                  as BS
import           Data.Default
import           Data.IORef                       (IORef, writeIORef)
import           Data.ProtocolBuffers
import           Data.Serialize.Get
import           Data.Serialize.Put
import           Data.Time.Clock.POSIX

import           Control.Exception
import           Control.Lens
import           Control.Monad

import           Network.Socket                   hiding (recv, recvFrom, send,
                                                   sendTo)
import           Network.Socket.ByteString


-- | High-level and probably too naive state of TCP connection to Riemann server.
data TCPState =  CnxClosed
                 -- ^Connection is known to be closed
              | CnxOpen (Socket, AddrInfo)
                -- ^Connection is expected to be usable with given socket and address
              | CnxError IOException
                -- ^Connection failed with given exception
              deriving Show

-- |Is given state in error?
isTCPError :: TCPState -> Bool
isTCPError (CnxError _) = True
isTCPError _            = False

-- | Try connecting with TCP to given host/port.
--
--  * If the passed @TCPState@ is already connected then no new connection attempt is made
--  * Otherwise, we try to connect to given host/port and update @TCPState@ accordingly
--
-- '''Note''': We never use IPv6 address resolved for given hostname.
tcpConnect :: Hostname -> Port -> TCPState -> IO TCPState
tcpConnect _ _  s@(CnxOpen _) = return s
tcpConnect hn po _            = do
  res <- try $ doConnect hn po
  return $ case res of
   Left e  -> CnxError e
   Right s -> CnxOpen s

tcpv4 :: AddrInfo -> Bool
tcpv4 addr = addrSocketType addr == Stream  &&
             addrFamily addr == AF_INET

doConnect :: HostName -> Port -> IO (Socket, AddrInfo)
doConnect hn po = do addrs <- getAddrInfo
                              (Just $ defaultHints {
                                  addrFlags = [AI_NUMERICSERV] })
                              (Just hn)
                              (Just $ show po)
                     case (filter tcpv4 addrs) of
                      []       -> fail ("No accessible addresses in " ++ show addrs)
                      (addy:_) -> do
                        s <- socket AF_INET
                             Stream
                             defaultProtocol
                        connect s (addrAddress addy)
                        return (s, addy)


-- | Try sending a riemann event to given @Socket@, updating given state accordingly.
doSendTCPEvent :: IORef TCPState -> Socket -> Event -> IO ()
doSendTCPEvent r s event = do
  sending <- try $ do
    now <- fmap round getPOSIXTime
    let msg = def & events .~ [event & time ?~ now]
        bytes = runPut $ encodeMessage msg
        bytesWithLen = runPut (putWord32be (fromIntegral $ BS.length bytes)  >> putByteString bytes)
    void $ send s bytesWithLen
  case sending of
   Left e  -> writeIORef r (CnxError e)
   Right _ -> doReceiveAck r s

-- | Re
doReceiveAck :: IORef TCPState -> Socket -> IO ()
doReceiveAck r s = do
  bs <- recv s 4096
  case decoded bs of
   Left err  -> return () -- TODO something useful, but what ? Close the socket ?
   Right msg -> do
     let st = msg ^. msgState
     case st of
      Ok      -> return ()
      Error t -> return () -- TODO something useful
      Unknown -> return ()

  where
    decoded bs = runGet decodeMessage bs :: Either String Msg
