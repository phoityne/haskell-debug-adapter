{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Haskell.Debug.Adapter.State.DebugRun where

import Control.Monad.IO.Class
import qualified System.Log.Logger as L
import Control.Monad.State.Lazy
import Control.Monad.Except
import Control.Lens
import qualified Text.Read as R
import qualified Data.List as L

import qualified Haskell.DAP as DAP
import Haskell.Debug.Adapter.Constant
import qualified Haskell.Debug.Adapter.Utility as U
import Haskell.Debug.Adapter.Type
import qualified Haskell.Debug.Adapter.GHCi as P
import Haskell.Debug.Adapter.State.DebugRun.Threads()
import Haskell.Debug.Adapter.State.DebugRun.StackTrace()
import Haskell.Debug.Adapter.State.DebugRun.Scopes()
import Haskell.Debug.Adapter.State.DebugRun.Variables()
import Haskell.Debug.Adapter.State.DebugRun.Continue()
import Haskell.Debug.Adapter.State.DebugRun.Next()
import Haskell.Debug.Adapter.State.DebugRun.StepIn()
import Haskell.Debug.Adapter.State.DebugRun.Terminate()
import Haskell.Debug.Adapter.State.DebugRun.InternalTerminate()
import qualified Haskell.Debug.Adapter.State.Utility as SU


instance AppStateIF DebugRunStateData where
  -- |
  --
  entryAction DebugRunState = do
    liftIO $ L.debugM _LOG_APP "DebugRunState entryAction called."
    goEntry

  -- |
  --
  exitAction DebugRunState = do
    liftIO $ L.debugM _LOG_APP "DebugRunState exitAction called."
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
instance StateActivityIF DebugRunStateData DAP.InitializeRequest

-- |
--   default nop.
--
instance StateActivityIF DebugRunStateData DAP.LaunchRequest

-- |
--   default nop.
--
instance StateActivityIF DebugRunStateData DAP.DisconnectRequest

-- |
--   default nop.
--
instance StateActivityIF DebugRunStateData DAP.PauseRequest

-- |
--
goEntry :: AppContext ()
goEntry = view stopOnEntryAppStores <$> get >>= \case
  True  -> stopOnEntry
  False -> startDebug

-- |
--
stopOnEntry :: AppContext ()
stopOnEntry = do
  startupFile <- view startupAppStores <$> get
  startupFunc <- view startupFuncAppStores <$> get

  let funcName = if null startupFunc then "main" else startupFunc
      funcBp   = (startupFile, DAP.FunctionBreakpoint funcName Nothing Nothing)
      cmd = ":dap-set-function-breakpoint " ++ U.showDAP funcBp

  P.cmdAndOut cmd
  res <- P.expectH P.stdoutCallBk

  withAdhocAddDapHeader $ filter (U.startswith _DAP_HEADER) res

  where
    -- |
    --
    withAdhocAddDapHeader :: [String] -> AppContext ()
    withAdhocAddDapHeader [] = do
      U.warnEV _LOG_APP $ "can not set func breakpoint. no dap header found."
      startDebug
    withAdhocAddDapHeader (str:[]) = case R.readEither (drop (length _DAP_HEADER) str) of
      Left err -> do
        U.warnEV _LOG_APP $ "read response body failed. " ++ err ++ " : " ++ str
        startDebug
      Right (Left err) -> do
        U.warnEV _LOG_APP $ "set adhoc breakpoint failed. " ++ err ++ " : " ++ str
        startDebug
      Right (Right bp) -> do
        startDebug
        adhocDelBreakpoint bp
    withAdhocAddDapHeader _ = do
      U.warnEV _LOG_APP $ "can not set func breakpoint. ambiguous dap header found."
      startDebug

    -- |
    --
    adhocDelBreakpoint :: DAP.Breakpoint -> AppContext ()
    adhocDelBreakpoint bp = do
      let cmd = ":dap-delete-breakpoint " ++ U.showDAP bp

      P.cmdAndOut cmd
      res <- P.expectH P.stdoutCallBk

      withAdhocDelDapHeader $ filter (U.startswith _DAP_HEADER) res

    -- |
    --
    withAdhocDelDapHeader :: [String] -> AppContext ()
    withAdhocDelDapHeader [] = throwError $ "can not del func breakpoint. no dap header found."
    withAdhocDelDapHeader (str:[]) = case R.readEither (drop (length _DAP_HEADER) str) of
      Left err -> throwError $ "read response body failed. " ++ err ++ " : " ++ str
      Right (Left err) -> throwError $ "del adhoc breakpoint failed. " ++ err ++ " : " ++ str
      Right (Right res) -> return res
    withAdhocDelDapHeader _ = throwError $ "can not del func breakpoint. ambiguous dap header found."


-- |
--
startDebug :: AppContext ()
startDebug = do
  expr <- getTraceExpr
  let args = DAP.defaultContinueRequestArguments {
             DAP.exprContinueRequestArguments = Just expr
           }

  startDebugDAP args

  where
    getTraceExpr = view startupFuncAppStores <$> get >>= \case
      [] -> return "main"
      func -> do
        funcArgs <- view startupArgsAppStores <$> get
        return $ U.strip $ func ++ " " ++ funcArgs

    startDebugDAP args = do

      let dap = ":dap-continue "
          cmd = dap ++ U.showDAP args
          dbg = dap ++ show args

      P.cmdAndOut cmd
      U.debugEV _LOG_APP dbg
      P.expectH $ P.funcCallBk lineCallBk

      return ()


    lineCallBk :: Bool -> String -> AppContext ()
    lineCallBk True  s = U.sendStdoutEvent s
    lineCallBk False s
      | L.isPrefixOf _DAP_HEADER s = do
        U.debugEV _LOG_APP s
        dapHdl $ drop (length _DAP_HEADER) s
      | otherwise = U.sendStdoutEventLF s

    -- |
    --
    dapHdl :: String -> AppContext ()
    dapHdl str = case R.readEither str of
      Left err -> errHdl str err
      Right (Left err) -> errHdl str err
      Right (Right body) -> U.handleStoppedEventBody body

    -- |
    --
    errHdl :: String -> String -> AppContext()
    errHdl str err = do
      let msg = "start debugging failed. " ++ err ++ " : " ++ str
      liftIO $ L.errorM _LOG_APP msg
      U.sendErrorEventLF msg


-- |
--  Any errors should be sent back as False result Response
--
instance StateActivityIF DebugRunStateData DAP.SetBreakpointsRequest where
  action _ (SetBreakpointsRequest req) = do
    liftIO $ L.debugM _LOG_APP $ "DebugRunState SetBreakpointsRequest called. " ++ show req
    SU.setBreakpointsRequest req

-- |
--  Any errors should be sent back as False result Response
--
instance StateActivityIF DebugRunStateData DAP.SetExceptionBreakpointsRequest where
  action _ (SetExceptionBreakpointsRequest req) = do
    liftIO $ L.debugM _LOG_APP $ "DebugRunState SetExceptionBreakpointsRequest called. " ++ show req
    SU.setExceptionBreakpointsRequest req

-- |
--  Any errors should be sent back as False result Response
--
instance StateActivityIF DebugRunStateData DAP.SetFunctionBreakpointsRequest where
  action _ (SetFunctionBreakpointsRequest req) = do
    liftIO $ L.debugM _LOG_APP $ "DebugRunState SetFunctionBreakpointsRequest called. " ++ show req
    SU.setFunctionBreakpointsRequest req

-- |
--   default nop.
--
instance StateActivityIF DebugRunStateData DAP.ConfigurationDoneRequest

-- |
--  Any errors should be sent back as False result Response
--
instance StateActivityIF DebugRunStateData DAP.EvaluateRequest where
  action _ (EvaluateRequest req) = do
    liftIO $ L.debugM _LOG_APP $ "DebugRunState EvaluateRequest called. " ++ show req
    SU.evaluateRequest req

-- |
--  Any errors should be sent back as False result Response
--
instance StateActivityIF DebugRunStateData DAP.CompletionsRequest where
  action _ (CompletionsRequest req) = do
    liftIO $ L.debugM _LOG_APP $ "DebugRunState CompletionsRequest called. " ++ show req
    SU.completionsRequest req

-- |
--   default nop.
--
instance StateActivityIF DebugRunStateData HdaInternalTransitRequest

-- |
--  Any errors should be sent back as False result Response
--
instance StateActivityIF DebugRunStateData HdaInternalLoadRequest where
  action _ (InternalLoadRequest req) = do
    liftIO $ L.debugM _LOG_APP $ "DebugRunState InternalLoadRequest called. " ++ show req
    SU.loadHsFile $ pathHdaInternalLoadRequest req
    return $ Just DebugRun_Contaminated

