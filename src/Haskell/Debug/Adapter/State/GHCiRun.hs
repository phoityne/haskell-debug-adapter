{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Haskell.Debug.Adapter.State.GHCiRun where

import Control.Monad.IO.Class
import qualified System.Log.Logger as L

import qualified GHCi.DAP as DAP
import Haskell.Debug.Adapter.Constant
import Haskell.Debug.Adapter.Type
import Haskell.Debug.Adapter.State.GHCiRun.ConfigurationDone()
import qualified Haskell.Debug.Adapter.State.Utility as SU
import qualified Haskell.Debug.Adapter.Utility as U

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
  getStateRequest GHCiRunState (WrapRequest (InitializeRequest req))              = SU.unsupported $ show req
  getStateRequest GHCiRunState (WrapRequest (LaunchRequest req))                  = SU.unsupported $ show req
  getStateRequest GHCiRunState (WrapRequest (DisconnectRequest req))              = SU.unsupported $ show req
  getStateRequest GHCiRunState (WrapRequest (PauseRequest req))                   = SU.unsupported $ show req
  
  getStateRequest GHCiRunState (WrapRequest (TerminateRequest req))               = return . WrapStateRequest $ GHCiRun_Terminate req
  getStateRequest GHCiRunState (WrapRequest (SetBreakpointsRequest req))          = return . WrapStateRequest $ GHCiRun_SetBreakpoints req
  getStateRequest GHCiRunState (WrapRequest (SetFunctionBreakpointsRequest req))  = return . WrapStateRequest $ GHCiRun_SetFunctionBreakpoints req
  getStateRequest GHCiRunState (WrapRequest (SetExceptionBreakpointsRequest req)) = return . WrapStateRequest $ GHCiRun_SetExceptionBreakpoints req
  getStateRequest GHCiRunState (WrapRequest (ConfigurationDoneRequest req))       = return . WrapStateRequest $ GHCiRun_ConfigurationDone req
  
  getStateRequest GHCiRunState (WrapRequest (ThreadsRequest req))                 = return . WrapStateRequest $ GHCiRun_Threads req
  getStateRequest GHCiRunState (WrapRequest (StackTraceRequest req))              = return . WrapStateRequest $ GHCiRun_StackTrace req
  getStateRequest GHCiRunState (WrapRequest (ScopesRequest req))                  = return . WrapStateRequest $ GHCiRun_Scopes req
  getStateRequest GHCiRunState (WrapRequest (VariablesRequest req))               = return . WrapStateRequest $ GHCiRun_Variables req

  getStateRequest GHCiRunState (WrapRequest (ContinueRequest req))                = return . WrapStateRequest $ GHCiRun_Continue req
  getStateRequest GHCiRunState (WrapRequest (NextRequest req))                    = return . WrapStateRequest $ GHCiRun_Next req
  getStateRequest GHCiRunState (WrapRequest (StepInRequest req))                  = return . WrapStateRequest $ GHCiRun_StepIn req

  getStateRequest GHCiRunState (WrapRequest (EvaluateRequest req))                = return . WrapStateRequest $ GHCiRun_Evaluate req
  getStateRequest GHCiRunState (WrapRequest (CompletionsRequest req))             = return . WrapStateRequest $ GHCiRun_Completions req

  getStateRequest GHCiRunState (WrapRequest (InternalTransitRequest req))         = SU.unsupported $ show req

  getStateRequest GHCiRunState (WrapRequest (InternalTerminateRequest req))       = return . WrapStateRequest $ GHCiRun_InternalTerminate req
  getStateRequest GHCiRunState (WrapRequest (InternalLoadRequest req))            = return . WrapStateRequest $ GHCiRun_InternalLoad req


-- |
--  Any errors should be sent back as False result Response
--
instance StateRequestIF GHCiRunState DAP.SetBreakpointsRequest where
  action (GHCiRun_SetBreakpoints req) = do
    liftIO $ L.debugM _LOG_APP $ "GHCiRunState SetBreakpointsRequest called. " ++ show req
    SU.setBreakpointsRequest req

-- |
--  Any errors should be sent back as False result Response
--
instance StateRequestIF GHCiRunState DAP.SetExceptionBreakpointsRequest where
  action (GHCiRun_SetExceptionBreakpoints req) = do
    liftIO $ L.debugM _LOG_APP $ "GHCiRunState SetExceptionBreakpointsRequest called. " ++ show req
    SU.setExceptionBreakpointsRequest req

-- |
--  Any errors should be sent back as False result Response
--
instance StateRequestIF GHCiRunState DAP.SetFunctionBreakpointsRequest where
  action (GHCiRun_SetFunctionBreakpoints req) = do
    liftIO $ L.debugM _LOG_APP $ "GHCiRunState SetFunctionBreakpointsRequest called. " ++ show req
    SU.setFunctionBreakpointsRequest req


-- |
--  Any errors should be sent back as False result Response
--
instance StateRequestIF GHCiRunState DAP.TerminateRequest where
  action (GHCiRun_Terminate req) = do
    liftIO $ L.debugM _LOG_APP $ "GHCiRunState TerminateRequest called. " ++ show req

    SU.terminateRequest req

    return $ Just GHCiRun_Shutdown


-- |
--  Any errors should be sent back as False result Response
--
instance StateRequestIF GHCiRunState DAP.ThreadsRequest where
  action (GHCiRun_Threads req) = do
    liftIO $ L.debugM _LOG_APP $ "GHCiRunState ThreadsRequest called. " ++ show req
    resSeq <- U.getIncreasedResponseSequence
    let res = DAP.defaultThreadsResponse {
              DAP.seqThreadsResponse = resSeq
            , DAP.request_seqThreadsResponse = DAP.seqThreadsRequest req
            , DAP.successThreadsResponse = False
            , DAP.messageThreadsResponse = "GHCiRun State. debugging not started."
            }

    U.addResponse $ ThreadsResponse res
    return Nothing



-- |
--  Any errors should be sent back as False result Response
--
instance StateRequestIF GHCiRunState DAP.StackTraceRequest where
  action (GHCiRun_StackTrace req) = do
    liftIO $ L.debugM _LOG_APP $ "GHCiRunState StackTraceRequest called. " ++ show req
    resSeq <- U.getIncreasedResponseSequence
    let res = DAP.defaultStackTraceResponse {
              DAP.seqStackTraceResponse = resSeq
            , DAP.request_seqStackTraceResponse = DAP.seqStackTraceRequest req
            , DAP.successStackTraceResponse = False
            , DAP.messageStackTraceResponse = "GHCiRun State. debugging not started."
            }

    U.addResponse $ StackTraceResponse res
    return Nothing



-- |
--  Any errors should be sent back as False result Response
--
instance StateRequestIF GHCiRunState DAP.ScopesRequest where
  action (GHCiRun_Scopes req) = do
    liftIO $ L.debugM _LOG_APP $ "GHCiRunState ScopesRequest called. " ++ show req
    resSeq <- U.getIncreasedResponseSequence
    let res = DAP.defaultScopesResponse {
              DAP.seqScopesResponse = resSeq
            , DAP.request_seqScopesResponse = DAP.seqScopesRequest req
            , DAP.successScopesResponse = False
            , DAP.messageScopesResponse = "GHCiRun State. debugging not started."
            }

    U.addResponse $ ScopesResponse res
    return Nothing



-- |
--  Any errors should be sent back as False result Response
--
instance StateRequestIF GHCiRunState DAP.VariablesRequest where
  action (GHCiRun_Variables req) = do
    liftIO $ L.debugM _LOG_APP $ "GHCiRunState VariablesRequest called. " ++ show req
    resSeq <- U.getIncreasedResponseSequence
    let res = DAP.defaultVariablesResponse {
              DAP.seqVariablesResponse = resSeq
            , DAP.request_seqVariablesResponse = DAP.seqVariablesRequest req
            , DAP.successVariablesResponse = False
            , DAP.messageVariablesResponse = "GHCiRun State. debugging not started."
            }

    U.addResponse $ VariablesResponse res
    return Nothing



-- |
--  Any errors should be sent back as False result Response
--
instance StateRequestIF GHCiRunState DAP.ContinueRequest where
  action (GHCiRun_Continue req) = do
    liftIO $ L.debugM _LOG_APP $ "GHCiRunState ContinueRequest called. " ++ show req
    resSeq <- U.getIncreasedResponseSequence
    let res = DAP.defaultContinueResponse {
              DAP.seqContinueResponse = resSeq
            , DAP.request_seqContinueResponse = DAP.seqContinueRequest req
            , DAP.successContinueResponse = True
            }

    U.addResponse $ ContinueResponse res

    return $ Just GHCiRun_DebugRun


-- |
--  Any errors should be sent back as False result Response
--
instance StateRequestIF GHCiRunState DAP.NextRequest where
  action (GHCiRun_Next req) = do
    liftIO $ L.debugM _LOG_APP $ "GHCiRunState NextRequest called. " ++ show req
    resSeq <- U.getIncreasedResponseSequence
    let res = DAP.defaultNextResponse {
              DAP.seqNextResponse = resSeq
            , DAP.request_seqNextResponse = DAP.seqNextRequest req
            , DAP.successNextResponse = True
            }

    U.addResponse $ NextResponse res

    return $ Just GHCiRun_DebugRun


-- |
--  Any errors should be sent back as False result Response
--
instance StateRequestIF GHCiRunState DAP.StepInRequest where
  action (GHCiRun_StepIn req) = do
    liftIO $ L.debugM _LOG_APP $ "GHCiRunState StepInRequest called. " ++ show req
    resSeq <- U.getIncreasedResponseSequence
    let res = DAP.defaultStepInResponse {
              DAP.seqStepInResponse = resSeq
            , DAP.request_seqStepInResponse = DAP.seqStepInRequest req
            , DAP.successStepInResponse = True
            }

    U.addResponse $ StepInResponse res

    return $ Just GHCiRun_DebugRun



-- |
--
instance StateRequestIF GHCiRunState DAP.EvaluateRequest where
  action (GHCiRun_Evaluate req) = do
    liftIO $ L.debugM _LOG_APP $ "GHCiRunState EvaluateRequest called. " ++ show req
    SU.evaluateRequest req

-- |
--
instance StateRequestIF GHCiRunState DAP.CompletionsRequest where
  action (GHCiRun_Completions req) = do
    liftIO $ L.debugM _LOG_APP $ "GHCiRunState CompletionsRequest called. " ++ show req
    SU.completionsRequest req


-- |
--   Any errors should be critical. don't catch anything here.
--
instance StateRequestIF GHCiRunState HdaInternalTerminateRequest where
  action (GHCiRun_InternalTerminate req) = do
    liftIO $ L.debugM _LOG_APP $ "GHCiRunState InternalTerminateRequest called. " ++ show req
    SU.internalTerminateRequest
    return $ Just GHCiRun_Shutdown
  

-- |
--
instance StateRequestIF GHCiRunState HdaInternalLoadRequest where
  action (GHCiRun_InternalLoad req) = do
    liftIO $ L.debugM _LOG_APP $ "GHCiRunState InternalLoadRequest called. " ++ show req
    SU.loadHsFile $ pathHdaInternalLoadRequest req
    return $ Just GHCiRun_Contaminated

