{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Haskell.Debug.Adapter.State.Init where

import Control.Monad.IO.Class
import Control.Monad.Except
import qualified System.Log.Logger as L
import qualified Haskell.DAP as DAP

import Haskell.Debug.Adapter.Type
import Haskell.Debug.Adapter.Constant
import Haskell.Debug.Adapter.State.Init.Initialize()
import Haskell.Debug.Adapter.State.Init.Launch()

import Haskell.Debug.Adapter.State.Init.McpInitialize()
import Haskell.Debug.Adapter.State.Init.McpToolsList()
import Haskell.Debug.Adapter.State.Init.McpCallTool()
import qualified Haskell.Debug.Adapter.MCP.Type as MCP

-- |
--
instance AppStateIF InitStateData where
  -- |
  --
  entryAction InitState = do
    let msg = "InitState entryAction must not be called."
    throwError msg

  -- |
  --
  exitAction InitState = do
    liftIO $ L.debugM _LOG_APP "InitState exitAction called."
    return ()

  -- |
  --
  doActivity s (WrapRequest r@McpInitializeRequest{})           = action s r
  doActivity s (WrapRequest r@McpInitializedNotification{})     = action s r
  doActivity s (WrapRequest r@McpToolsListRequest{})     = action s r
  doActivity s (WrapRequest r@McpCallToolRequest{})     = action s r


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
--   Any errors should be critical. don't catch anything here.
--
instance StateActivityIF InitStateData MCP.McpInitializedNotification where
  action _ (McpInitializedNotification _) = do
    --stdioLogging $ str2lbs $ "[INFO] mcp initialized notification request called.\n"
    return Nothing




-- |
--   default nop.
--
instance StateActivityIF InitStateData DAP.DisconnectRequest

-- |
--   default nop.
--
instance StateActivityIF InitStateData DAP.PauseRequest

-- |
--   default nop.
--
instance StateActivityIF InitStateData DAP.TerminateRequest

-- |
--   default nop.
--
instance StateActivityIF InitStateData DAP.SetBreakpointsRequest

-- |
--   default nop.
--
instance StateActivityIF InitStateData DAP.SetFunctionBreakpointsRequest

-- |
--   default nop.
--
instance StateActivityIF InitStateData DAP.SetExceptionBreakpointsRequest

-- |
--   default nop.
--
instance StateActivityIF InitStateData DAP.ConfigurationDoneRequest

-- |
--   default nop.
--
instance StateActivityIF InitStateData DAP.ThreadsRequest

-- |
--   default nop.
--
instance StateActivityIF InitStateData DAP.StackTraceRequest

-- |
--   default nop.
--
instance StateActivityIF InitStateData DAP.ScopesRequest

-- |
--   default nop.
--
instance StateActivityIF InitStateData DAP.VariablesRequest

-- |
--   default nop.
--
instance StateActivityIF InitStateData DAP.SourceRequest

-- |
--   default nop.
--
instance StateActivityIF InitStateData DAP.ContinueRequest

-- |
--   default nop.
--
instance StateActivityIF InitStateData DAP.NextRequest

-- |
--   default nop.
--
instance StateActivityIF InitStateData DAP.StepInRequest

-- |
--   default nop.
--
instance StateActivityIF InitStateData DAP.EvaluateRequest

-- |
--   default nop.
--
instance StateActivityIF InitStateData DAP.CompletionsRequest

-- |
--   default nop.
--
instance StateActivityIF InitStateData HdaInternalTransitRequest

-- |
--   default nop.
--
instance StateActivityIF InitStateData HdaInternalTerminateRequest

-- |
--   default nop.
--
instance StateActivityIF InitStateData HdaInternalLoadRequest


