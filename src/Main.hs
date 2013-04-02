{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Main (main) where

import Control.Concurrent
import System.Win32.SystemServices.Services
import System.Win32.Types

main :: IO ()
main = do
    gState <- newMVar (1, SERVICE_STATUS WIN32_OWN_PROCESS
                          START_PENDING [] nO_ERROR 0 0 3000)
    mStop <- newEmptyMVar
    startServiceCtrlDispatcher "sudo4win" 3000
        (svcCtrlHandler mStop gState)
        (svcMain        mStop gState)


svcMain :: MVar () -> MVar (DWORD, SERVICE_STATUS) -> ServiceMainFunction
svcMain mStop gState _ _ h = do
    reportSvcStatus h RUNNING nO_ERROR 0 gState
    appendFile "c:\\test.txt" "1"
    threadDelay 1000000
    appendFile "c:\\test.txt" "2"
    threadDelay 1000000
    appendFile "c:\\test.txt" "3"
    threadDelay 1000000
    appendFile "c:\\test.txt" "4"
    threadDelay 1000000
    appendFile "c:\\test.txt" "5"
    takeMVar mStop
    threadDelay 1000000
    appendFile "c:\\test.txt" "."
    threadDelay 1000000
    appendFile "c:\\test.txt" "."
    threadDelay 1000000
    appendFile "c:\\test.txt" "."
    threadDelay 1000000
    appendFile "c:\\test.txt" "Stahp\n"
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
