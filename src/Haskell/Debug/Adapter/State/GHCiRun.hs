{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Haskell.Debug.Adapter.State.GHCiRun where

import Control.Monad.IO.Class
import qualified System.Log.Logger as L
import Control.Monad.Except

import Haskell.Debug.Adapter.Constant
import Haskell.Debug.Adapter.Type
import Haskell.Debug.Adapter.State.GHCiRun.Initialize()
import Haskell.Debug.Adapter.State.GHCiRun.Launch()
import Haskell.Debug.Adapter.State.GHCiRun.Disconnect()
import Haskell.Debug.Adapter.State.GHCiRun.SetBreakpoints()
import Haskell.Debug.Adapter.State.GHCiRun.SetFunctionBreakpoints()
import Haskell.Debug.Adapter.State.GHCiRun.SetExceptionBreakpoints()
import Haskell.Debug.Adapter.State.GHCiRun.ConfigurationDone()

instance AppStateIF GHCiRunState where
  -- |
  --
  entryAction GHCiRunState = do
    liftIO $ L.debugM _LOG_APP "GHCiRunState entryAction called."
    return ()

  -- |
  --
  exitAction GHCiRunState = do
    liftIO $ L.debugM _LOG_APP "GHCiRunState exitAction called."
    return ()


  -- | 
  --
  getStateRequest GHCiRunState (WrapRequest (InitializeRequest req)) = return . WrapStateRequest $ GHCiRun_Initialize req
  getStateRequest GHCiRunState (WrapRequest (LaunchRequest req))     = return . WrapStateRequest $ GHCiRun_Launch req
  getStateRequest GHCiRunState (WrapRequest (DisconnectRequest req)) = return . WrapStateRequest $ GHCiRun_Disconnect req
  getStateRequest GHCiRunState (WrapRequest (SetBreakpointsRequest req)) = return . WrapStateRequest $ GHCiRun_SetBreakpoints req
  getStateRequest GHCiRunState (WrapRequest (SetFunctionBreakpointsRequest req)) = return . WrapStateRequest $ GHCiRun_SetFunctionBreakpoints req
  getStateRequest GHCiRunState (WrapRequest (SetExceptionBreakpointsRequest req)) = return . WrapStateRequest $ GHCiRun_SetExceptionBreakpoints req
  getStateRequest GHCiRunState (WrapRequest (ConfigurationDoneRequest req)) = return . WrapStateRequest $ GHCiRun_ConfigurationDone req
  getStateRequest GHCiRunState (WrapRequest (ThreadsRequest req)) = do
    let msg = "GHCiRunState does not support this request. " ++ show req
    liftIO $ L.criticalM _LOG_APP msg
    throwError msg

  getStateRequest GHCiRunState (WrapRequest (StackTraceRequest req)) = do
    let msg = "GHCiRunState does not support this request. " ++ show req
    liftIO $ L.criticalM _LOG_APP msg
    throwError msg

  getStateRequest GHCiRunState (WrapRequest (ScopesRequest req)) = do
    let msg = "GHCiRunState does not support this request. " ++ show req
    liftIO $ L.criticalM _LOG_APP msg
    throwError msg

  getStateRequest GHCiRunState (WrapRequest (VariablesRequest req)) = do
    let msg = "GHCiRunState does not support this request. " ++ show req
    liftIO $ L.criticalM _LOG_APP msg
    throwError msg

  getStateRequest GHCiRunState (WrapRequest (ContinueRequest req)) = do
    let msg = "GHCiRunState does not support this request. " ++ show req
    liftIO $ L.criticalM _LOG_APP msg
    throwError msg

  getStateRequest GHCiRunState (WrapRequest (NextRequest req)) = do
    let msg = "GHCiRunState does not support this request. " ++ show req
    liftIO $ L.criticalM _LOG_APP msg
    throwError msg

  getStateRequest GHCiRunState (WrapRequest (StepInRequest req)) = do
    let msg = "GHCiRunState does not support this request. " ++ show req
    liftIO $ L.criticalM _LOG_APP msg
    throwError msg
        