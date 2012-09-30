module Main (main) where

import Control.Concurrent
import Data.IORef
import System.Win32.Service

main :: IO ()
main = do
    (loop, stop) <- loopStop go
    service "sudo" loop stop
  where
    go = do appendFile "c:\\test.txt" "whee"
            threadDelay 1000000

loopStop :: IO () -> IO (IO (), IO ())
loopStop once = do
    ref <- newIORef False
    let stop = writeIORef ref True
        loop = do done <- readIORef ref
                  if done then return ()
                          else once >> loop
    return (loop, stop)
