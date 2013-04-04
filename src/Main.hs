{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Main (main) where

import           Control.Applicative ((<$>))
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.MVar
import           Control.Exception (throw)
import           Control.Monad (forever, when)
import           Data.Binary
import qualified Data.ByteString as B
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import           System.Directory (getCurrentDirectory)
import           System.Environment (getArgs, getExecutablePath, getEnvironment)
import           System.Exit
import           System.IO (Handle, BufferMode(..))
import           System.IO (stdout, stderr, hSetBuffering, hSetBinaryMode)
import           System.Process hiding (runProcess)
import           System.Win32.SystemServices.Services
import           System.Win32.Types

import           Control.Distributed.Process
import           Control.Distributed.Process.Node
import           Control.Distributed.Process.Closure
import           Network (withSocketsDo)
import           Network.Transport.TCP

------------------------------------------------------------------------
-- Remotable

data Command = Command {
      cmdPid    :: ProcessId
    , cmdArgs   :: [String]
    , cmdDir    :: FilePath
    , cmdEnv    :: [(String, String)]
    , cmdStdOut :: SendPort B.ByteString
    , cmdStdErr :: SendPort B.ByteString
    } deriving (Show, Generic, Typeable)

command :: Command -> Process ExitCode
command Command{..} = do
    let cmd = unwords cmdArgs
    liftIO $ putStrLn (cmdDir ++ " $ " ++ cmd)

    (Nothing, Just hOut, Just hErr, pid) <- liftIO $ createProcess
        (shell cmd) {
          cwd     = Just cmdDir
        , env     = Just cmdEnv
        , std_out = CreatePipe
        , std_err = CreatePipe
        }

    link cmdPid

    spawnReader hOut cmdStdOut
    spawnReader hErr cmdStdErr

    liftIO $ waitForProcess pid

spawnWriter :: Handle -> Process (SendPort B.ByteString)
spawnWriter h = do
    liftIO (hSetBuffering h NoBuffering)
    liftIO (hSetBinaryMode h True)
    (s, r) <- newChan
    spawnLocal $ forever $ do
        bs <- receiveChan r
        liftIO (B.hPut h bs)
        when (B.null bs) terminate
    return s

spawnReader :: Handle -> SendPort B.ByteString -> Process ()
spawnReader h port = do
    liftIO (hSetBuffering h NoBuffering)
    liftIO (hSetBinaryMode h True)
    spawnLocal $ forever $ do
        bs <- liftIO $ B.hGet h (64 * 1024)
        sendChan port bs
        when (B.null bs) terminate
    return ()

------------------------------------------------------------------------

deriving instance Generic ExitCode

instance Binary Command
instance Binary ExitCode

$(remotable ['command])

command' :: Command -> Closure (Process ExitCode)
command' = $(mkClosure 'command)

remoteTable :: RemoteTable
remoteTable = __remoteTable initRemoteTable

------------------------------------------------------------------------
-- Main

main :: IO ()
main = withSocketsDo $ do
    args <- getArgs
    case args of
      ["--service"] -> runService
      ["--server"]  -> runServer
      _             -> runClient args

------------------------------------------------------------------------
-- Client

runClient :: [String] -> IO ()
runClient args = do
    transport <- either throw id <$>
                 createTransport "127.0.0.1" "30021" defaultTCPParameters
    local  <- newLocalNode transport remoteTable
    server <- readServerNode
    env    <- getEnvironment
    dir    <- getCurrentDirectory
    runProcess local $ do
        pid <- getSelfPid

        outPort <- spawnWriter stdout
        errPort <- spawnWriter stderr

        let cmd = Command pid args dir env outPort errPort
        code <- call $(functionTDict 'command) server (command' cmd)

        liftIO (exitWith code)

------------------------------------------------------------------------
-- Server

runServer :: IO ()
runServer = do
    startServerNode
    putStrLn "Running sudo server..."
    forever $ threadDelay 1000000

startServerNode :: IO ()
startServerNode = do
    transport <- either throw id <$>
                 createTransport "127.0.0.1" "30020" defaultTCPParameters
    node <- newLocalNode transport remoteTable
    writeServerNode (localNodeId node)

writeServerNode :: NodeId -> IO ()
writeServerNode nid = do
    path <- getExecutablePath
    encodeFile (path ++ ".nid") nid

readServerNode :: IO NodeId
readServerNode = do
    path <- getExecutablePath
    decodeFile (path ++ ".nid")

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
    startServerNode
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
