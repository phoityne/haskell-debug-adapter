{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Haskell.Debug.Adapter.State.Contaminated where

import Control.Monad.IO.Class
import qualified System.Log.Logger as L

import qualified Haskell.DAP as DAP
import Haskell.Debug.Adapter.Type
import qualified Haskell.Debug.Adapter.Utility as U
import Haskell.Debug.Adapter.Constant
import qualified Haskell.Debug.Adapter.State.Utility as SU


-- |
--
instance AppStateIF ContaminatedStateData where
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
  doActivity s (WrapRequest r@InitializeRequest{})              = action s r
  doActivity s (WrapRequest r@LaunchRequest{})                  = action s r
  doActivity s (WrapRequest r@DisconnectRequest{})              = action s r
  doActivity s (WrapRequest r@PauseRequest{})                   = action s r
  doActivity s (WrapRequest r@TerminateRequest{})               = action s r
  doActivity s (WrapRequest r@SetBreakpointsRequest{})          = action s r
  doActivity s (WrapRequest r@SetFunctionBreakpointsRequest{})  = action s r
  doActivity s (WrapRequest r@SetExceptionBreakpointsRequest{}) = action s r
  doActivity s (WrapRequest r@ConfigurationDoneRequest{})       = action s r
  doActivity s (WrapRequest r@ThreadsRequest{})                 = action s r
  doActivity s (WrapRequest r@StackTraceRequest{})              = action s r
  doActivity s (WrapRequest r@ScopesRequest{})                  = action s r
  doActivity s (WrapRequest r@VariablesRequest{})               = action s r
  doActivity s (WrapRequest r@SourceRequest{})                  = action s r
  doActivity s (WrapRequest r@ContinueRequest{})                = action s r
  doActivity s (WrapRequest r@NextRequest{})                    = action s r
  doActivity s (WrapRequest r@StepInRequest{})                  = action s r
  doActivity s (WrapRequest r@EvaluateRequest{})                = action s r
  doActivity s (WrapRequest r@CompletionsRequest{})             = action s r
  doActivity s (WrapRequest r@InternalTransitRequest{})         = action s r
  doActivity s (WrapRequest r@InternalTerminateRequest{})       = action s r
  doActivity s (WrapRequest r@InternalLoadRequest{})            = action s r

-- |
--   default nop.
--
instance StateActivityIF ContaminatedStateData DAP.InitializeRequest

-- |
--   default nop.
--
instance StateActivityIF ContaminatedStateData DAP.LaunchRequest

-- |
--   default nop.
--
instance StateActivityIF ContaminatedStateData DAP.DisconnectRequest

-- |
--   default nop.
--
instance StateActivityIF ContaminatedStateData DAP.PauseRequest

-- |
--  Any errors should be sent back as False result Response
--
instance StateActivityIF ContaminatedStateData DAP.TerminateRequest where
  action _ (TerminateRequest req) = do
    liftIO $ L.debugM _LOG_APP $ "ContaminatedState TerminateRequest called. " ++ show req

    SU.terminateRequest req

    return $ Just Contaminated_Shutdown

-- |
--  Any errors should be sent back as False result Response
--
instance StateActivityIF ContaminatedStateData DAP.SetBreakpointsRequest where
  action _ (SetBreakpointsRequest req) = do
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
--  Any errors should be sent back as False result Response
--
instance StateActivityIF ContaminatedStateData DAP.SetFunctionBreakpointsRequest where
  action _ (SetFunctionBreakpointsRequest req) = do
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
--  Any errors should be sent back as False result Response
--
instance StateActivityIF ContaminatedStateData DAP.SetExceptionBreakpointsRequest where
  action _ (SetExceptionBreakpointsRequest req) = do
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
--   default nop.
--
instance StateActivityIF ContaminatedStateData DAP.ConfigurationDoneRequest

-- |
--  Any errors should be sent back as False result Response
--
instance StateActivityIF ContaminatedStateData DAP.ThreadsRequest where
  action _ (ThreadsRequest req) = do
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
--  Any errors should be sent back as False result Response
--
instance StateActivityIF ContaminatedStateData DAP.StackTraceRequest where
  action _ (StackTraceRequest req) = do
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
--  Any errors should be sent back as False result Response
--
instance StateActivityIF ContaminatedStateData DAP.ScopesRequest where
  action _ (ScopesRequest req) = do
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
--  Any errors should be sent back as False result Response
--
instance StateActivityIF ContaminatedStateData DAP.VariablesRequest where
  action _ (VariablesRequest req) = do
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
--  Any errors should be sent back as False result Response
--
instance StateActivityIF ContaminatedStateData DAP.SourceRequest where
  action _ (SourceRequest req) = do
    resSeq <- U.getIncreasedResponseSequence
    let res = DAP.defaultSourceResponse {
        DAP.seqSourceResponse = resSeq
      , DAP.request_seqSourceResponse = DAP.seqSourceRequest req
      , DAP.successSourceResponse = False
      , DAP.messageSourceResponse = "Contaminated State. need restart."
      }

    U.addResponse $ SourceResponse res
    return Nothing

-- |
--  Any errors should be sent back as False result Response
--
instance StateActivityIF ContaminatedStateData DAP.ContinueRequest where
  action _ (ContinueRequest req) = do
    liftIO $ L.debugM _LOG_APP $ "ContaminatedState ContinueRequest called. " ++ show req
    restartEvent
    return $ Just Contaminated_Shutdown

-- |
--  Any errors should be sent back as False result Response
--
instance StateActivityIF ContaminatedStateData DAP.NextRequest where
  action _ (NextRequest req) = do
    liftIO $ L.debugM _LOG_APP $ "ContaminatedState NextRequest called. " ++ show req
    restartEvent
    return $ Just Contaminated_Shutdown

-- |
--  Any errors should be sent back as False result Response
--
instance StateActivityIF ContaminatedStateData DAP.StepInRequest where
  action _ (StepInRequest req) = do
    liftIO $ L.debugM _LOG_APP $ "ContaminatedState StepInRequest called. " ++ show req
    restartEvent
    return $ Just Contaminated_Shutdown

-- |
--  Any errors should be sent back as False result Response
--
instance StateActivityIF ContaminatedStateData DAP.EvaluateRequest where
  action _ (EvaluateRequest req) = do
    liftIO $ L.debugM _LOG_APP $ "ContaminatedState EvaluateRequest called. " ++ show req
    SU.evaluateRequest req

-- |
--  Any errors should be sent back as False result Response
--
instance StateActivityIF ContaminatedStateData DAP.CompletionsRequest where
  action _ (CompletionsRequest req) = do
    liftIO $ L.debugM _LOG_APP $ "ContaminatedState CompletionsRequest called. " ++ show req
    SU.completionsRequest req

-- |
--   default nop.
--
instance StateActivityIF ContaminatedStateData HdaInternalTransitRequest

-- |
--  Any errors should be sent back as False result Response
--
instance StateActivityIF ContaminatedStateData HdaInternalTerminateRequest where
  action _ (InternalTerminateRequest req) = do
    liftIO $ L.debugM _LOG_APP $ "ContaminatedState InternalTerminateRequest called. " ++ show req
    SU.internalTerminateRequest
    return $ Just Contaminated_Shutdown

-- |
--  Any errors should be sent back as False result Response
--
instance StateActivityIF ContaminatedStateData HdaInternalLoadRequest where
  action _ (InternalLoadRequest req) = do
    liftIO $ L.debugM _LOG_APP $ "ContaminatedState InternalLoadRequest called. " ++ show req
    SU.loadHsFile $ pathHdaInternalLoadRequest req
    return Nothing

-- |
--
restartEvent :: AppContext ()
restartEvent = do
  SU.terminateGHCi

  U.sendConsoleEventLF ""
  U.sendConsoleEventLF "restarting debug adapter."
  U.sendConsoleEventLF ""

  U.sendRestartEvent
  U.sendExitedEvent

