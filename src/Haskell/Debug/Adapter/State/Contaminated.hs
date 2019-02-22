{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Haskell.Debug.Adapter.State.Contaminated where

import Control.Monad.IO.Class
--import Control.Monad.Except
import qualified System.Log.Logger as L
import Control.Concurrent (threadDelay)

import qualified GHCi.DAP as DAP
import Haskell.Debug.Adapter.Type
import qualified Haskell.Debug.Adapter.Utility as U
import Haskell.Debug.Adapter.Constant
import qualified Haskell.Debug.Adapter.State.Utility as SU


-- | 
--
instance AppStateIF ContaminatedState where
  -- |
  --
  entryAction ContaminatedState = do
    liftIO $ L.debugM _LOG_APP "ContaminatedState entryAction called."
    return ()

    
  -- |
  --
  exitAction ContaminatedState = do
    liftIO $ L.debugM _LOG_APP "ContaminatedState exitAction called."
    return ()


  -- | 
  --
  getStateRequest ContaminatedState (WrapRequest (InitializeRequest req))              = SU.unsupported $ show req
  getStateRequest ContaminatedState (WrapRequest (LaunchRequest req))                  = SU.unsupported $ show req
  getStateRequest ContaminatedState (WrapRequest (DisconnectRequest req))              = SU.unsupported $ show req
  getStateRequest ContaminatedState (WrapRequest (TerminateRequest req))               = return . WrapStateRequest $ Contaminated_Terminate req
  
  getStateRequest ContaminatedState (WrapRequest (SetBreakpointsRequest req))          = return . WrapStateRequest $ Contaminated_SetBreakpoints req
  getStateRequest ContaminatedState (WrapRequest (SetFunctionBreakpointsRequest req))  = return . WrapStateRequest $ Contaminated_SetFunctionBreakpoints req
  getStateRequest ContaminatedState (WrapRequest (SetExceptionBreakpointsRequest req)) = return . WrapStateRequest $ Contaminated_SetExceptionBreakpoints req
  getStateRequest ContaminatedState (WrapRequest (ConfigurationDoneRequest req))       = SU.unsupported $ show req
  getStateRequest ContaminatedState (WrapRequest (ThreadsRequest req))                 = return . WrapStateRequest $ Contaminated_Threads req
  getStateRequest ContaminatedState (WrapRequest (StackTraceRequest req))              = return . WrapStateRequest $ Contaminated_StackTrace req
  getStateRequest ContaminatedState (WrapRequest (ScopesRequest req))                  = return . WrapStateRequest $ Contaminated_Scopes req
  getStateRequest ContaminatedState (WrapRequest (VariablesRequest req))               = return . WrapStateRequest $ Contaminated_Variables req
  getStateRequest ContaminatedState (WrapRequest (ContinueRequest req))                = return . WrapStateRequest $ Contaminated_Continue req
  getStateRequest ContaminatedState (WrapRequest (NextRequest req))                    = return . WrapStateRequest $ Contaminated_Next req
  getStateRequest ContaminatedState (WrapRequest (StepInRequest req))                  = return . WrapStateRequest $ Contaminated_StepIn req
  getStateRequest ContaminatedState (WrapRequest (EvaluateRequest req))                = return . WrapStateRequest $ Contaminated_Evaluate req
  getStateRequest ContaminatedState (WrapRequest (CompletionsRequest req))             = return . WrapStateRequest $ Contaminated_Completions req
  getStateRequest ContaminatedState (WrapRequest (InternalTransitRequest req))         = SU.unsupported $ show req
  getStateRequest ContaminatedState (WrapRequest (InternalTerminateRequest req))       = return . WrapStateRequest $ Contaminated_InternalTerminate req
  getStateRequest ContaminatedState (WrapRequest (InternalLoadRequest req))            = return . WrapStateRequest $ Contaminated_InternalLoad req


-- |
--  Any errors should be send back as False result Response
--
instance StateRequestIF ContaminatedState DAP.TerminateRequest where
  action (Contaminated_Terminate req) = do
    liftIO $ L.debugM _LOG_APP $ "ContaminatedState TerminateRequest called. " ++ show req

    SU.terminateRequest req

    return $ Just Contaminated_Shutdown


-- |
--  Any errors should be send back as False result Response
--
instance StateRequestIF ContaminatedState DAP.SetBreakpointsRequest where
  action (Contaminated_SetBreakpoints req) = do
    liftIO $ L.debugM _LOG_APP $ "ContaminatedState SetBreakpointsRequest called. " ++ show req
    resSeq <- U.getIncreasedResponseSequence
    let res = DAP.defaultSetBreakpointsResponse {
              DAP.seqSetBreakpointsResponse = resSeq
            , DAP.request_seqSetBreakpointsResponse = DAP.seqSetBreakpointsRequest req
            , DAP.successSetBreakpointsResponse = False
            , DAP.messageSetBreakpointsResponse = "Contaminated State. need restart."
            }

    U.addResponse $ SetBreakpointsResponse res
    return Nothing


-- |
--  Any errors should be send back as False result Response
--
instance StateRequestIF ContaminatedState DAP.SetFunctionBreakpointsRequest where
  action (Contaminated_SetFunctionBreakpoints req) = do
    liftIO $ L.debugM _LOG_APP $ "ContaminatedState SetFunctionBreakpointsRequest called. " ++ show req
    resSeq <- U.getIncreasedResponseSequence
    let res = DAP.defaultSetFunctionBreakpointsResponse {
              DAP.seqSetFunctionBreakpointsResponse = resSeq
            , DAP.request_seqSetFunctionBreakpointsResponse = DAP.seqSetFunctionBreakpointsRequest req
            , DAP.successSetFunctionBreakpointsResponse = False
            , DAP.messageSetFunctionBreakpointsResponse = "Contaminated State. need restart."
            }

    U.addResponse $ SetFunctionBreakpointsResponse res
    return Nothing


-- |
--  Any errors should be send back as False result Response
--
instance StateRequestIF ContaminatedState DAP.SetExceptionBreakpointsRequest where
  action (Contaminated_SetExceptionBreakpoints req) = do
    liftIO $ L.debugM _LOG_APP $ "ContaminatedState SetExceptionBreakpointsRequest called. " ++ show req
    resSeq <- U.getIncreasedResponseSequence
    let res = DAP.defaultSetExceptionBreakpointsResponse {
              DAP.seqSetExceptionBreakpointsResponse = resSeq
            , DAP.request_seqSetExceptionBreakpointsResponse = DAP.seqSetExceptionBreakpointsRequest req
            , DAP.successSetExceptionBreakpointsResponse = False
            , DAP.messageSetExceptionBreakpointsResponse = "Contaminated State. need restart."
            }

    U.addResponse $ SetExceptionBreakpointsResponse res
    return Nothing


-- |
--  Any errors should be send back as False result Response
--
instance StateRequestIF ContaminatedState DAP.ThreadsRequest where
  action (Contaminated_Threads req) = do
    liftIO $ L.debugM _LOG_APP $ "ContaminatedState ThreadsRequest called. " ++ show req
    resSeq <- U.getIncreasedResponseSequence
    let res = DAP.defaultThreadsResponse {
              DAP.seqThreadsResponse = resSeq
            , DAP.request_seqThreadsResponse = DAP.seqThreadsRequest req
            , DAP.successThreadsResponse = True
            , DAP.messageThreadsResponse = "Contaminated State. need restart."
            }

    U.addResponse $ ThreadsResponse res
    return Nothing


-- |
--  Any errors should be send back as False result Response
--
instance StateRequestIF ContaminatedState DAP.StackTraceRequest where
  action (Contaminated_StackTrace req) = do
    liftIO $ L.debugM _LOG_APP $ "ContaminatedState StackTraceRequest called. " ++ show req
    resSeq <- U.getIncreasedResponseSequence
    let res = DAP.defaultStackTraceResponse {
              DAP.seqStackTraceResponse = resSeq
            , DAP.request_seqStackTraceResponse = DAP.seqStackTraceRequest req
            , DAP.successStackTraceResponse = False
            , DAP.messageStackTraceResponse = "Contaminated State. need restart."
            }

    U.addResponse $ StackTraceResponse res
    return Nothing



-- |
--  Any errors should be send back as False result Response
--
instance StateRequestIF ContaminatedState DAP.ScopesRequest where
  action (Contaminated_Scopes req) = do
    liftIO $ L.debugM _LOG_APP $ "ContaminatedState ScopesRequest called. " ++ show req
    resSeq <- U.getIncreasedResponseSequence
    let res = DAP.defaultScopesResponse {
              DAP.seqScopesResponse = resSeq
            , DAP.request_seqScopesResponse = DAP.seqScopesRequest req
            , DAP.successScopesResponse = False
            , DAP.messageScopesResponse = "Contaminated State. need restart."
            }

    U.addResponse $ ScopesResponse res
    return Nothing



-- |
--  Any errors should be send back as False result Response
--
instance StateRequestIF ContaminatedState DAP.VariablesRequest where
  action (Contaminated_Variables req) = do
    liftIO $ L.debugM _LOG_APP $ "ContaminatedState VariablesRequest called. " ++ show req
    resSeq <- U.getIncreasedResponseSequence
    let res = DAP.defaultVariablesResponse {
              DAP.seqVariablesResponse = resSeq
            , DAP.request_seqVariablesResponse = DAP.seqVariablesRequest req
            , DAP.successVariablesResponse = False
            , DAP.messageVariablesResponse = "Contaminated State. need restart."
            }

    U.addResponse $ VariablesResponse res
    return Nothing


-- |
--  Any errors should be send back as False result Response
--
instance StateRequestIF ContaminatedState DAP.ContinueRequest where
  action (Contaminated_Continue req) = do
    liftIO $ L.debugM _LOG_APP $ "ContaminatedState ContinueRequest called. " ++ show req
    restartEvent
    return $ Just Contaminated_Shutdown


-- |
--  Any errors should be send back as False result Response
--
instance StateRequestIF ContaminatedState DAP.NextRequest where
  action (Contaminated_Next req) = do
    liftIO $ L.debugM _LOG_APP $ "ContaminatedState NextRequest called. " ++ show req
    restartEvent
    return $ Just Contaminated_Shutdown


-- |
--  Any errors should be send back as False result Response
--
instance StateRequestIF ContaminatedState DAP.StepInRequest where
  action (Contaminated_StepIn req) = do
    liftIO $ L.debugM _LOG_APP $ "ContaminatedState StepInRequest called. " ++ show req
    restartEvent
    return $ Just Contaminated_Shutdown


-- |
--  Any errors should be send back as False result Response
--
instance StateRequestIF ContaminatedState DAP.EvaluateRequest where
  action (Contaminated_Evaluate req) = do
    liftIO $ L.debugM _LOG_APP $ "ContaminatedState EvaluateRequest called. " ++ show req
    SU.evaluateRequest req


-- |
--
instance StateRequestIF ContaminatedState DAP.CompletionsRequest where
  action (Contaminated_Completions req) = do
    liftIO $ L.debugM _LOG_APP $ "ContaminatedState CompletionsRequest called. " ++ show req
    SU.completionsRequest req

-- |
--   Any errors should be critical. don't catch anything here.
--
instance StateRequestIF ContaminatedState HdaInternalTerminateRequest where
  action (Contaminated_InternalTerminate req) = do
    liftIO $ L.debugM _LOG_APP $ "ContaminatedState InternalTerminateRequest called. " ++ show req
    SU.internalTerminateRequest
    return $ Just Contaminated_Shutdown
  

-- |
--
instance StateRequestIF ContaminatedState HdaInternalLoadRequest where
  action (Contaminated_InternalLoad req) = do
    liftIO $ L.debugM _LOG_APP $ "ContaminatedState InternalLoadRequest called. " ++ show req
    SU.loadHsFile $ pathHdaInternalLoadRequest req
    return Nothing

-- |
--
restartEvent :: AppContext ()
restartEvent = do
  SU.terminateGHCi
  liftIO $ threadDelay _1_SEC

  U.sendConsoleEventLF ""
  U.sendConsoleEventLF "restarting debug adapter."
  U.sendConsoleEventLF ""

  U.sendRestartEvent
  U.sendExitedEvent

