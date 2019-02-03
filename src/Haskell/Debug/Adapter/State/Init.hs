{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Haskell.Debug.Adapter.State.Init where

import Control.Monad.IO.Class
import Control.Monad.Except
import qualified System.Log.Logger as L

import Haskell.Debug.Adapter.Type
import Haskell.Debug.Adapter.Constant
import Haskell.Debug.Adapter.State.Init.Initialize()
import Haskell.Debug.Adapter.State.Init.Launch()
import Haskell.Debug.Adapter.State.Init.Disconnect()


-- | 
--
instance AppStateIF InitState where
  -- |
  --
  entryAction InitState = do
    let msg = "InitState entryAction must not be called."
    liftIO $ L.criticalM _LOG_APP msg
    throwError msg

  -- |
  --
  exitAction InitState = do
    liftIO $ L.debugM _LOG_APP "InitState exitAction called."
    return ()

  -- | 
  --
  getStateRequest InitState (WrapRequest (InitializeRequest req)) = return . WrapStateRequest $ Init_Initialize req
  getStateRequest InitState (WrapRequest (LaunchRequest req))     = return . WrapStateRequest $ Init_Launch req
  getStateRequest InitState (WrapRequest (DisconnectRequest req)) = return . WrapStateRequest $ Init_Disconnect req
  getStateRequest InitState (WrapRequest (SetBreakpointsRequest req)) = do
    let msg = "InitState does not support this request. " ++ show req
    liftIO $ L.criticalM _LOG_APP msg
    throwError msg
  getStateRequest InitState (WrapRequest (SetFunctionBreakpointsRequest req)) = do
    let msg = "InitState does not support this request. " ++ show req
    liftIO $ L.criticalM _LOG_APP msg
    throwError msg
  getStateRequest InitState (WrapRequest (SetExceptionBreakpointsRequest req)) = do
    let msg = "InitState does not support this request. " ++ show req
    liftIO $ L.criticalM _LOG_APP msg
    throwError msg
  getStateRequest InitState (WrapRequest (ConfigurationDoneRequest req)) = do
    let msg = "InitState does not support this request. " ++ show req
    liftIO $ L.criticalM _LOG_APP msg
    throwError msg

  getStateRequest InitState (WrapRequest (ThreadsRequest req)) = unsupported $ show req
  getStateRequest InitState (WrapRequest (StackTraceRequest req)) = unsupported $ show req
  getStateRequest InitState (WrapRequest (ScopesRequest req)) = unsupported $ show req
  getStateRequest InitState (WrapRequest (VariablesRequest req)) = unsupported $ show req
  getStateRequest InitState (WrapRequest (ContinueRequest req)) = unsupported $ show req
  getStateRequest InitState (WrapRequest (NextRequest req)) = unsupported $ show req
  getStateRequest InitState (WrapRequest (StepInRequest req)) = unsupported $ show req


-- | 
--
unsupported :: String -> AppContext WrapStateRequest
unsupported reqStr = do
  let msg = "InitState does not support this request. " ++ reqStr
  liftIO $ L.criticalM _LOG_APP msg
  throwError msg
