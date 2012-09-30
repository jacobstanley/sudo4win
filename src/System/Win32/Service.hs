{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Win32.Service (
      service
    ) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.IORef
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import System.Win32.Types

------------------------------------------------------------------------
-- Types

data Status =
      Stopped
    | StartPending
    | StopPending
    | Running
    | ContinuePending
    | PausePending
    | Paused
    deriving (Eq, Show)

------------------------------------------------------------------------
-- Functions

service :: String -> IO () -> IO () -> IO ()
service name loop stop =
    withTString name $ \namePtr -> do
    procPtr <- toProcPtr (proc namePtr)
    failIfFalse_ "service" (c'svc_start_dispatcher namePtr procPtr)
    freeHaskellFunPtr procPtr
  where
    proc :: LPCTSTR -> ProcFun
    proc namePtr _ _ = do
        ref <- newIORef nullHANDLE
        handlerPtr <- toHandlerPtr (handler ref)
        h <- c'RegisterServiceCtrlHandler namePtr handlerPtr
        when (h /= nullHANDLE) $ do
          writeIORef ref h
          stopOnError h $ do
            setStatus h 0 Running
            loop
            setStatus h 0 Stopped
        freeHaskellFunPtr handlerPtr

    stopOnError h =
        handle (\(_::SomeException) -> setStatus h (-1) Stopped)

    handler :: IORef HANDLE -> HandlerFun
    handler ref x
        | x == 0x1  = stop'
        | x == 0x5  = stop'
        | otherwise = return ()
      where
        stop' = do
            h <- readIORef ref
            setStatus h 0 StopPending
            stop

setStatus :: HANDLE -> DWORD -> Status -> IO ()
setStatus h exitCode status =
    with newStatus $ \ptr -> do
    _ <- c'SetServiceStatus h ptr
    return ()
  where
    statusCode = case status of
        Stopped         -> 1
        StartPending    -> 2
        StopPending     -> 3
        Running         -> 4
        ContinuePending -> 5
        PausePending    -> 6
        Paused          -> 7

    newStatus = defaultStatus { dwCurrentState  = statusCode
                              , dwWin32ExitCode = exitCode }

------------------------------------------------------------------------
-- FFI

foreign import ccall "svc_start_dispatcher"
    c'svc_start_dispatcher :: LPTSTR -> FunPtr ProcFun -> IO Bool

-- SERVICE_STATUS_HANDLE RegisterServiceCtrlHandler(
--   LPCTSTR lpServiceName, LPHANDLER_FUNCTION lpHandlerProc);
foreign import stdcall "windows.h RegisterServiceCtrlHandlerW"
    c'RegisterServiceCtrlHandler :: LPCTSTR -> FunPtr HandlerFun -> IO HANDLE

-- BOOL SetServiceStatus(
--   SERVICE_STATUS_HANDLE hServiceStatus, LPSERVICE_STATUS lpServiceStatus);
foreign import stdcall "windows.h SetServiceStatus"
    c'SetServiceStatus :: HANDLE -> Ptr SERVICE_STATUS -> IO BOOL

-- void ServiceMain(DWORD dwArgc, LPTSTR *lpszArgv);
type ProcFun = DWORD -> Ptr LPTSTR -> IO ()
foreign import ccall "wrapper"
    toProcPtr :: ProcFun -> IO (FunPtr ProcFun)

-- void Handler(DWORD fdwControl);
type HandlerFun = DWORD -> IO ()
foreign import ccall "wrapper"
    toHandlerPtr :: HandlerFun -> IO (FunPtr HandlerFun)

------------------------------------------------------------------------

data SERVICE_STATUS = SERVICE_STATUS {
      dwServiceType             :: DWORD
    , dwCurrentState            :: DWORD
    , dwControlsAccepted        :: DWORD
    , dwWin32ExitCode           :: DWORD
    , dwServiceSpecificExitCode :: DWORD
    , dwCheckPoint              :: DWORD
    , dwWaitHint                :: DWORD
    }

defaultStatus :: SERVICE_STATUS
defaultStatus = SERVICE_STATUS {
      dwServiceType      = 0x10 -- SERVICE_WIN32_OWN_PROCESS
    , dwCurrentState     = 0x2  -- SERVICE_START_PENDING
    , dwControlsAccepted = 0x5  -- SERVICE_ACCEPT_STOP | SERVICE_ACCEPT_SHUTDOWN
    , dwWin32ExitCode    = 0
    , dwServiceSpecificExitCode = 0
    , dwCheckPoint       = 0
    , dwWaitHint         = 0
    }

instance Storable SERVICE_STATUS where
  sizeOf _ = 28
  alignment _ = 4
  peek ptr = SERVICE_STATUS
      <$> peek (castPtr ptr)
      <*> peek (castPtr ptr `plusPtr` 4)
      <*> peek (castPtr ptr `plusPtr` 8)
      <*> peek (castPtr ptr `plusPtr` 12)
      <*> peek (castPtr ptr `plusPtr` 16)
      <*> peek (castPtr ptr `plusPtr` 20)
      <*> peek (castPtr ptr `plusPtr` 24)
  poke ptr (SERVICE_STATUS a b c d e f g) = do
      poke (castPtr ptr) a
      poke (castPtr ptr `plusPtr` 4)  b
      poke (castPtr ptr `plusPtr` 8)  c
      poke (castPtr ptr `plusPtr` 12) d
      poke (castPtr ptr `plusPtr` 16) e
      poke (castPtr ptr `plusPtr` 20) f
      poke (castPtr ptr `plusPtr` 24) g
