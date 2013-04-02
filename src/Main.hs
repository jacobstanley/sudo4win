{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Main (main) where

import Control.Applicative ((<$>))
import Control.Exception (throw)
import Control.Concurrent
import Control.Monad (forever)
import System.Environment (getArgs)
import System.Win32.SystemServices.Services
import System.Win32.Types

import Control.Distributed.Process
import Control.Distributed.Process.Node
import Network.Transport.TCP

import Network (withSocketsDo)

main :: IO ()
main = withSocketsDo $ do
    args <- getArgs
    case args of
      ["--service"] -> runService
      ["--server"]  -> runServer
      xs            -> runClient xs

------------------------------------------------------------------------
-- Client

runClient :: [String] -> IO ()
runClient _ = do
    transport <- either throw id <$>
                 createTransport "localhost" "30020" defaultTCPParameters
    node <- newLocalNode transport initRemoteTable
    runProcess node $ do
        return ()

------------------------------------------------------------------------
-- Server

runServer :: IO ()
runServer = do
    forkServer
    putStrLn "Running sudo server..."
    forever $ threadDelay 1000000

forkServer :: IO ProcessId
forkServer = do
    transport <- either throw id <$>
                 createTransport "localhost" "30020" defaultTCPParameters
    node <- newLocalNode transport initRemoteTable
    print (localNodeId node)
    forkProcess node $ forever $ do
        str <- expect
        liftIO (putStrLn str)

------------------------------------------------------------------------
-- Windows Service

runService :: IO ()
runService = do
    gState <- newMVar (1, SERVICE_STATUS WIN32_OWN_PROCESS START_PENDING [] nO_ERROR 0 0 3000)
    mStop  <- newEmptyMVar
    startServiceCtrlDispatcher "sudo4win" 3000
        (svcCtrlHandler mStop gState)
        (svcMain        mStop gState)

svcMain :: MVar () -> MVar (DWORD, SERVICE_STATUS) -> ServiceMainFunction
svcMain mStop gState _ _ h = do
    reportSvcStatus h RUNNING nO_ERROR 0 gState
    forkServer
    takeMVar mStop
    reportSvcStatus h STOPPED nO_ERROR 0 gState

reportSvcStatus :: HANDLE -> SERVICE_STATE -> DWORD -> DWORD
    -> MVar (DWORD, SERVICE_STATUS) -> IO ()
reportSvcStatus hStatus state win32ExitCode waitHint gState = do
    modifyMVar_ gState $ \(checkPoint, svcStatus) -> do
        let state' = nextState (checkPoint, svcStatus
             { win32ExitCode = win32ExitCode
             , waitHint      = waitHint
             , currentState  = state })
        setServiceStatus hStatus (snd state')
        return state'

nextState :: (DWORD, SERVICE_STATUS) -> (DWORD, SERVICE_STATUS)
nextState (checkPoint, svcStatus) = case (currentState svcStatus) of
    START_PENDING -> (checkPoint + 1, svcStatus
        { controlsAccepted = [], checkPoint = checkPoint + 1 })
    RUNNING -> (checkPoint, svcStatus
        { controlsAccepted = [ACCEPT_STOP], checkPoint = 0 })
    STOPPED -> (checkPoint, svcStatus
        { controlsAccepted = [], checkPoint = 0 })
    _ -> (checkPoint + 1, svcStatus
        { controlsAccepted = [], checkPoint = checkPoint + 1 })

svcCtrlHandler :: MVar () -> MVar (DWORD, SERVICE_STATUS) -> HandlerFunction
svcCtrlHandler mStop gState hStatus STOP = do
    reportSvcStatus hStatus STOP_PENDING nO_ERROR 3000 gState
    putMVar mStop ()
    return True
svcCtrlHandler _ _ _ INTERROGATE = return True
svcCtrlHandler _ _ _ _  = return False
