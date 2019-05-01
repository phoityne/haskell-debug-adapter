{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Haskell.Debug.Adapter.State.Contaminated where

import Control.Monad.IO.Class
--import Control.Monad.Except
import qualified System.Log.Logger as L
import Control.Concurrent (threadDelay)

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
  doActivity s (WrapRequest r@InitializeRequest{})              = action2 s r
  doActivity s (WrapRequest r@LaunchRequest{})                  = action2 s r
  doActivity s (WrapRequest r@DisconnectRequest{})              = action2 s r
  doActivity s (WrapRequest r@PauseRequest{})                   = action2 s r
  doActivity s (WrapRequest r@TerminateRequest{})               = action2 s r
  doActivity s (WrapRequest r@SetBreakpointsRequest{})          = action2 s r
  doActivity s (WrapRequest r@SetFunctionBreakpointsRequest{})  = action2 s r
  doActivity s (WrapRequest r@SetExceptionBreakpointsRequest{}) = action2 s r
  doActivity s (WrapRequest r@ConfigurationDoneRequest{})       = action2 s r
  doActivity s (WrapRequest r@ThreadsRequest{})                 = action2 s r
  doActivity s (WrapRequest r@StackTraceRequest{})              = action2 s r
  doActivity s (WrapRequest r@ScopesRequest{})                  = action2 s r
  doActivity s (WrapRequest r@VariablesRequest{})               = action2 s r
  doActivity s (WrapRequest r@ContinueRequest{})                = action2 s r
  doActivity s (WrapRequest r@NextRequest{})                    = action2 s r
  doActivity s (WrapRequest r@StepInRequest{})                  = action2 s r
  doActivity s (WrapRequest r@EvaluateRequest{})                = action2 s r
  doActivity s (WrapRequest r@CompletionsRequest{})             = action2 s r
  doActivity s (WrapRequest r@InternalTransitRequest{})         = action2 s r
  doActivity s (WrapRequest r@InternalTerminateRequest{})       = action2 s r
  doActivity s (WrapRequest r@InternalLoadRequest{})            = action2 s r

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
  action2 _ (TerminateRequest req) = do
    liftIO $ L.debugM _LOG_APP $ "ContaminatedState TerminateRequest called. " ++ show req

    SU.terminateRequest req

    return $ Just Contaminated_Shutdown

-- |
--  Any errors should be sent back as False result Response
--
instance StateActivityIF ContaminatedStateData DAP.SetBreakpointsRequest where
  action2 _ (SetBreakpointsRequest req) = do
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
  action2 _ (SetFunctionBreakpointsRequest req) = do
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
  action2 _ (SetExceptionBreakpointsRequest req) = do
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
  action2 _ (ThreadsRequest req) = do
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
  action2 _ (StackTraceRequest req) = do
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
  action2 _ (ScopesRequest req) = do
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
  action2 _ (VariablesRequest req) = do
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
instance StateActivityIF ContaminatedStateData DAP.ContinueRequest where
  action2 _ (ContinueRequest req) = do
    liftIO $ L.debugM _LOG_APP $ "ContaminatedState ContinueRequest called. " ++ show req
    restartEvent
    return $ Just Contaminated_Shutdown

-- |
--  Any errors should be sent back as False result Response
--
instance StateActivityIF ContaminatedStateData DAP.NextRequest where
  action2 _ (NextRequest req) = do
    liftIO $ L.debugM _LOG_APP $ "ContaminatedState NextRequest called. " ++ show req
    restartEvent
    return $ Just Contaminated_Shutdown

-- |
--  Any errors should be sent back as False result Response
--
instance StateActivityIF ContaminatedStateData DAP.StepInRequest where
  action2 _ (StepInRequest req) = do
    liftIO $ L.debugM _LOG_APP $ "ContaminatedState StepInRequest called. " ++ show req
    restartEvent
    return $ Just Contaminated_Shutdown

-- |
--  Any errors should be sent back as False result Response
--
instance StateActivityIF ContaminatedStateData DAP.EvaluateRequest where
  action2 _ (EvaluateRequest req) = do
    liftIO $ L.debugM _LOG_APP $ "ContaminatedState EvaluateRequest called. " ++ show req
    SU.evaluateRequest req

-- |
--  Any errors should be sent back as False result Response
--
instance StateActivityIF ContaminatedStateData DAP.CompletionsRequest where
  action2 _ (CompletionsRequest req) = do
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
  action2 _ (InternalTerminateRequest req) = do
    liftIO $ L.debugM _LOG_APP $ "ContaminatedState InternalTerminateRequest called. " ++ show req
    SU.internalTerminateRequest
    return $ Just Contaminated_Shutdown

-- |
--  Any errors should be sent back as False result Response
--
instance StateActivityIF ContaminatedStateData HdaInternalLoadRequest where
  action2 _ (InternalLoadRequest req) = do
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

