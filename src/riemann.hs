{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Applicative
import           Control.Concurrent
import           Control.Error
import           Control.Exception          (catch)
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Monoid                ((<>))
import           Data.Time.Clock
import           Network.HTTP.Client        (HttpException (..))
import           Network.Monitoring.Riemann
import           Network.Wreq
import           System.Environment

main :: IO ()
main = do
  [rhost,rport,uriList] <- getArgs
  c <- makeTCPClient rhost (read rport)
  uris <- lines <$> readFile uriList
  forever $ checkAll c uris


checkAll :: Client -> [ String ] -> IO ()
checkAll c uris =  forM_ uris (check c) >> threadDelay (60 * 1000000)

check :: Client -> String -> IO ()
check c uri = do
  putStr $ "checking " <> uri <> " "
  st <- getCurrentTime
  ok <- (get uri >> return True) `catch` \ (_ :: HttpException) -> return False
  en <- getCurrentTime
  let event = ev ("http " <> uri) (realToFrac (diffUTCTime en st) :: Double) & state ?~ if ok then "ok" else "error"
  res <- liftIO $ runEitherT $ sendEvent' c event
  -- not very useful error handling, if socket fails to connect at startup it cannot reconnects later
  either
    (\e -> putStrLn ("error connecting to riemann server: " <> show e))
    (const $ putStrLn "sent event to riemann")
    res

