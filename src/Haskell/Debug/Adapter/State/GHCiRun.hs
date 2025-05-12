{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Haskell.Debug.Adapter.State.GHCiRun where

import Control.Monad.IO.Class
import qualified System.Log.Logger as L

import qualified Haskell.DAP as DAP
import Haskell.Debug.Adapter.Constant
import Haskell.Debug.Adapter.Type
import Haskell.Debug.Adapter.State.GHCiRun.ConfigurationDone()
import qualified Haskell.Debug.Adapter.State.Utility as SU
import qualified Haskell.Debug.Adapter.Utility as U
import qualified Haskell.Debug.Adapter.MCP.Type as MCP

instance AppStateIF GHCiRunStateData where
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

  doActivity s (WrapRequest r@McpInitializeRequest{})           = action s r
  doActivity s (WrapRequest r@McpInitializedNotification{})     = action s r
  doActivity s (WrapRequest r@McpToolsListRequest{})            = action s r
  doActivity s (WrapRequest r@McpCallToolRequest{})             = action s r

-- |
--   default nop.
--
instance StateActivityIF GHCiRunStateData MCP.McpInitializeRequest
-- |
--   default nop.
--
instance StateActivityIF GHCiRunStateData MCP.McpInitializedNotification
-- |
--   default nop.
--
instance StateActivityIF GHCiRunStateData MCP.McpToolsListRequest
-- |
--   default nop.
--
instance StateActivityIF GHCiRunStateData MCP.McpCallToolRequest

-- |
--   default nop.
--
instance StateActivityIF GHCiRunStateData DAP.InitializeRequest

-- |
--   default nop.
--
instance StateActivityIF GHCiRunStateData DAP.LaunchRequest

-- |
--   default nop.
--
instance StateActivityIF GHCiRunStateData DAP.DisconnectRequest

-- |
--   default nop.
--
instance StateActivityIF GHCiRunStateData DAP.PauseRequest

-- |
--  Any errors should be sent back as False result Response
--
instance StateActivityIF GHCiRunStateData DAP.TerminateRequest where
  action _ (TerminateRequest req) = do
    liftIO $ L.debugM _LOG_APP $ "GHCiRunState TerminateRequest called. " ++ show req

    SU.terminateRequest req

    return $ Just GHCiRun_Shutdown

-- |
--  Any errors should be sent back as False result Response
--
instance StateActivityIF GHCiRunStateData DAP.SetBreakpointsRequest where
  action _ (SetBreakpointsRequest req) = do
    liftIO $ L.debugM _LOG_APP $ "GHCiRunState SetBreakpointsRequest called. " ++ show req
    SU.setBreakpointsRequest req

-- |
--  Any errors should be sent back as False result Response
--
instance StateActivityIF GHCiRunStateData DAP.SetExceptionBreakpointsRequest where
  action _ (SetExceptionBreakpointsRequest req) = do
    liftIO $ L.debugM _LOG_APP $ "GHCiRunState SetExceptionBreakpointsRequest called. " ++ show req
    SU.setExceptionBreakpointsRequest req

-- |
--  Any errors should be sent back as False result Response
--
instance StateActivityIF GHCiRunStateData DAP.SetFunctionBreakpointsRequest where
  action _ (SetFunctionBreakpointsRequest req) = do
    liftIO $ L.debugM _LOG_APP $ "GHCiRunState SetFunctionBreakpointsRequest called. " ++ show req
    SU.setFunctionBreakpointsRequest req

-- |
--  Any errors should be sent back as False result Response
--
instance StateActivityIF GHCiRunStateData DAP.ThreadsRequest where
  action _ (ThreadsRequest req) = do
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
instance StateActivityIF GHCiRunStateData DAP.StackTraceRequest where
  action _ (StackTraceRequest req) = do
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
instance StateActivityIF GHCiRunStateData DAP.ScopesRequest where
  action _ (ScopesRequest req) = do
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
instance StateActivityIF GHCiRunStateData DAP.VariablesRequest where
  action _ (VariablesRequest req) = do
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
--   default nop.
--
instance StateActivityIF GHCiRunStateData DAP.SourceRequest


-- |
--  Any errors should be sent back as False result Response
--
instance StateActivityIF GHCiRunStateData DAP.ContinueRequest where
  action _ (ContinueRequest req) = do
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
instance StateActivityIF GHCiRunStateData DAP.NextRequest where
  action _ (NextRequest req) = do
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
instance StateActivityIF GHCiRunStateData DAP.StepInRequest where
  action _ (StepInRequest req) = do
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
--  Any errors should be sent back as False result Response
--
instance StateActivityIF GHCiRunStateData DAP.EvaluateRequest where
  action _ (EvaluateRequest req) = do
    liftIO $ L.debugM _LOG_APP $ "GHCiRunState EvaluateRequest called. " ++ show req
    SU.evaluateRequest req

-- |
--  Any errors should be sent back as False result Response
--
instance StateActivityIF GHCiRunStateData DAP.CompletionsRequest where
  action _ (CompletionsRequest req) = do
    liftIO $ L.debugM _LOG_APP $ "GHCiRunState CompletionsRequest called. " ++ show req
    SU.completionsRequest req

-- |
--   default nop.
--
instance StateActivityIF GHCiRunStateData HdaInternalTransitRequest

-- |
--  Any errors should be sent back as False result Response
--
instance StateActivityIF GHCiRunStateData HdaInternalTerminateRequest where
  action _ (InternalTerminateRequest req) = do
    liftIO $ L.debugM _LOG_APP $ "GHCiRunState InternalTerminateRequest called. " ++ show req
    SU.internalTerminateRequest
    return $ Just GHCiRun_Shutdown

-- |
--  Any errors should be sent back as False result Response
--
instance StateActivityIF GHCiRunStateData HdaInternalLoadRequest where
  action _ (InternalLoadRequest req) = do
    liftIO $ L.debugM _LOG_APP $ "GHCiRunState InternalTerminateRequest called. " ++ show req
    SU.internalTerminateRequest
    return $ Just GHCiRun_Shutdown


