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


