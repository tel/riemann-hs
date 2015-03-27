module Main where

import           Control.Monad
import           Network.Monitoring.Riemann
import           System.Environment

main :: IO ()
main = do
  [h,p,n] <- getArgs
  c <- makeClient h (read p)
  forM_ [1 .. read n :: Int ] (sendEvent c . ev "myservice")

