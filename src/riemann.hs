module Main where

import           Control.Concurrent
import           Control.Monad
import           Network.Monitoring.Riemann
import           System.Environment

main :: IO ()
main = do
  [h,p,n] <- getArgs
  c <- makeTCPClient h (read p)
  forM_ [1 .. read n :: Int ] (\k -> (sendEvent c $ ev "myservice" k) >> threadDelay 1000000)

