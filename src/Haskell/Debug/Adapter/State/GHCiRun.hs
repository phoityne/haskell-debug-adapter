{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Haskell.Debug.Adapter.State.GHCiRun where

import Control.Monad.IO.Class
import qualified System.Log.Logger as L

import qualified GHCi.DAP as DAP
import Haskell.Debug.Adapter.Constant
import Haskell.Debug.Adapter.Type
import Haskell.Debug.Adapter.State.GHCiRun.Disconnect()
import Haskell.Debug.Adapter.State.GHCiRun.ConfigurationDone()
import qualified Haskell.Debug.Adapter.State.Utility as SU

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
  getStateRequest GHCiRunState (WrapRequest (InitializeRequest req)) = SU.unsupported $ show req
  getStateRequest GHCiRunState (WrapRequest (LaunchRequest req))     = SU.unsupported $ show req
  
  getStateRequest GHCiRunState (WrapRequest (DisconnectRequest req)) = return . WrapStateRequest $ GHCiRun_Disconnect req
  getStateRequest GHCiRunState (WrapRequest (TerminateRequest req)) = SU.unsupported $ show req
  getStateRequest GHCiRunState (WrapRequest (SetBreakpointsRequest req)) = return . WrapStateRequest $ GHCiRun_SetBreakpoints req
  getStateRequest GHCiRunState (WrapRequest (SetFunctionBreakpointsRequest req)) = return . WrapStateRequest $ GHCiRun_SetFunctionBreakpoints req
  getStateRequest GHCiRunState (WrapRequest (SetExceptionBreakpointsRequest req)) = return . WrapStateRequest $ GHCiRun_SetExceptionBreakpoints req
  getStateRequest GHCiRunState (WrapRequest (ConfigurationDoneRequest req)) = return . WrapStateRequest $ GHCiRun_ConfigurationDone req
  
  getStateRequest GHCiRunState (WrapRequest (ThreadsRequest req)) = SU.unsupported $ show req
  getStateRequest GHCiRunState (WrapRequest (StackTraceRequest req)) = SU.unsupported $ show req
  getStateRequest GHCiRunState (WrapRequest (ScopesRequest req)) = SU.unsupported $ show req
  getStateRequest GHCiRunState (WrapRequest (VariablesRequest req)) = SU.unsupported $ show req
  getStateRequest GHCiRunState (WrapRequest (ContinueRequest req)) = SU.unsupported $ show req
  getStateRequest GHCiRunState (WrapRequest (NextRequest req)) = SU.unsupported $ show req
  getStateRequest GHCiRunState (WrapRequest (StepInRequest req)) = SU.unsupported $ show req
  getStateRequest GHCiRunState (WrapRequest (TransitRequest req)) = SU.unsupported $ show req
  getStateRequest GHCiRunState (WrapRequest (ShutdownRequest req)) = SU.unsupported $ show req


-- |
--  Any errors should be send back as False result Response
--
instance StateRequestIF GHCiRunState DAP.SetBreakpointsRequest where
  action (GHCiRun_SetBreakpoints req) = do
    liftIO $ L.debugM _LOG_APP $ "GHCiRunState SetBreakpointsRequest called. " ++ show req
    SU.setBreakpointsRequest req

-- |
--  Any errors should be send back as False result Response
--
instance StateRequestIF GHCiRunState DAP.SetExceptionBreakpointsRequest where
  action (GHCiRun_SetExceptionBreakpoints req) = do
    liftIO $ L.debugM _LOG_APP $ "GHCiRunState SetExceptionBreakpointsRequest called. " ++ show req
    SU.setExceptionBreakpointsRequest req

-- |
--  Any errors should be send back as False result Response
--
instance StateRequestIF GHCiRunState DAP.SetFunctionBreakpointsRequest where
  action (GHCiRun_SetFunctionBreakpoints req) = do
    liftIO $ L.debugM _LOG_APP $ "GHCiRunState SetFunctionBreakpointsRequest called. " ++ show req
    SU.setFunctionBreakpointsRequest req


  