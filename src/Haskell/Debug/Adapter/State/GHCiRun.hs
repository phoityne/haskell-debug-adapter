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
  action2 _ (TerminateRequest req) = do
    liftIO $ L.debugM _LOG_APP $ "GHCiRunState TerminateRequest called. " ++ show req

    SU.terminateRequest req

    return $ Just GHCiRun_Shutdown

-- |
--  Any errors should be sent back as False result Response
--
instance StateActivityIF GHCiRunStateData DAP.SetBreakpointsRequest where
  action2 _ (SetBreakpointsRequest req) = do
    liftIO $ L.debugM _LOG_APP $ "GHCiRunState SetBreakpointsRequest called. " ++ show req
    SU.setBreakpointsRequest req

-- |
--  Any errors should be sent back as False result Response
--
instance StateActivityIF GHCiRunStateData DAP.SetExceptionBreakpointsRequest where
  action2 _ (SetExceptionBreakpointsRequest req) = do
    liftIO $ L.debugM _LOG_APP $ "GHCiRunState SetExceptionBreakpointsRequest called. " ++ show req
    SU.setExceptionBreakpointsRequest req

-- |
--  Any errors should be sent back as False result Response
--
instance StateActivityIF GHCiRunStateData DAP.SetFunctionBreakpointsRequest where
  action2 _ (SetFunctionBreakpointsRequest req) = do
    liftIO $ L.debugM _LOG_APP $ "GHCiRunState SetFunctionBreakpointsRequest called. " ++ show req
    SU.setFunctionBreakpointsRequest req

-- |
--  Any errors should be sent back as False result Response
--
instance StateActivityIF GHCiRunStateData DAP.ThreadsRequest where
  action2 _ (ThreadsRequest req) = do
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
  action2 _ (StackTraceRequest req) = do
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
  action2 _ (ScopesRequest req) = do
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
  action2 _ (VariablesRequest req) = do
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
instance StateActivityIF GHCiRunStateData DAP.ContinueRequest where
  action2 _ (ContinueRequest req) = do
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
  action2 _ (NextRequest req) = do
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
  action2 _ (StepInRequest req) = do
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
  action2 _ (EvaluateRequest req) = do
    liftIO $ L.debugM _LOG_APP $ "GHCiRunState EvaluateRequest called. " ++ show req
    SU.evaluateRequest req

-- |
--  Any errors should be sent back as False result Response
--
instance StateActivityIF GHCiRunStateData DAP.CompletionsRequest where
  action2 _ (CompletionsRequest req) = do
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
  action2 _ (InternalTerminateRequest req) = do
    liftIO $ L.debugM _LOG_APP $ "GHCiRunState InternalTerminateRequest called. " ++ show req
    SU.internalTerminateRequest
    return $ Just GHCiRun_Shutdown

-- |
--  Any errors should be sent back as False result Response
--
instance StateActivityIF GHCiRunStateData HdaInternalLoadRequest where
  action2 _ (InternalLoadRequest req) = do
    liftIO $ L.debugM _LOG_APP $ "GHCiRunState InternalTerminateRequest called. " ++ show req
    SU.internalTerminateRequest
    return $ Just GHCiRun_Shutdown


