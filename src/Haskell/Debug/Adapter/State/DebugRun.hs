{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Haskell.Debug.Adapter.State.DebugRun where

import Control.Monad.IO.Class
import qualified System.Log.Logger as L
import Control.Monad.State.Lazy
import Control.Lens
import qualified Text.Read as R
import qualified Data.List as L
import qualified Data.String.Utils as U

import qualified GHCi.DAP as DAP
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


instance AppStateIF DebugRunState where
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
  getStateRequest DebugRunState (WrapRequest (InitializeRequest req)) = SU.unsupported $ show req
  getStateRequest DebugRunState (WrapRequest (LaunchRequest req)) = SU.unsupported $ show req
  getStateRequest DebugRunState (WrapRequest (DisconnectRequest req)) = SU.unsupported $ show req
  
  getStateRequest DebugRunState (WrapRequest (TerminateRequest req)) = return . WrapStateRequest $ DebugRun_Terminate req
  getStateRequest DebugRunState (WrapRequest (SetBreakpointsRequest req)) = return . WrapStateRequest $ DebugRun_SetBreakpoints req
  getStateRequest DebugRunState (WrapRequest (SetFunctionBreakpointsRequest req)) = return . WrapStateRequest $ DebugRun_SetFunctionBreakpoints req
  getStateRequest DebugRunState (WrapRequest (SetExceptionBreakpointsRequest req)) = return . WrapStateRequest $ DebugRun_SetExceptionBreakpoints req

  getStateRequest DebugRunState (WrapRequest (ConfigurationDoneRequest req)) = SU.unsupported $ show req

  getStateRequest DebugRunState (WrapRequest (ThreadsRequest req)) = return . WrapStateRequest $ DebugRun_Threads req
  getStateRequest DebugRunState (WrapRequest (StackTraceRequest req)) = return . WrapStateRequest $ DebugRun_StackTrace req
  getStateRequest DebugRunState (WrapRequest (ScopesRequest req)) = return . WrapStateRequest $ DebugRun_Scopes req
  getStateRequest DebugRunState (WrapRequest (VariablesRequest req)) = return . WrapStateRequest $ DebugRun_Variables req
  getStateRequest DebugRunState (WrapRequest (ContinueRequest req)) = return . WrapStateRequest $ DebugRun_Continue req
  getStateRequest DebugRunState (WrapRequest (NextRequest req)) = return . WrapStateRequest $ DebugRun_Next req
  getStateRequest DebugRunState (WrapRequest (StepInRequest req)) = return . WrapStateRequest $ DebugRun_StepIn req

  getStateRequest DebugRunState (WrapRequest (InternalTransitRequest req)) = SU.unsupported $ show req
  getStateRequest DebugRunState (WrapRequest (InternalTerminateRequest req)) = return . WrapStateRequest $ DebugRun_InternalTerminate req

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
  undefined


-- |
--
startDebug :: AppContext ()
startDebug = do
  expr <- getTraceExpr
  let args = DAP.defaultContinueArguments {
      DAP.exprContinueArguments = Just expr
    }

  startDebugDAP args

  where
    getTraceExpr = view startupFuncAppStores <$> get >>= \case
      [] -> return "main"
      func -> do
        funcArgs <- view startupArgsAppStores <$> get
        return $ U.strip $ func ++ " " ++ funcArgs

    startDebugDAP args = do
        
      let cmd = ":dap-continue " ++ U.showDAP args

      P.cmdAndOut cmd
      P.expectH $ P.funcCallBk lineCallBk

    
    lineCallBk :: Bool -> String -> AppContext ()
    lineCallBk True  s = U.sendStdoutEvent s
    lineCallBk False s
      | L.isPrefixOf _DAP_HEADER s = dapHdl $ drop (length _DAP_HEADER) s
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

      -- TODO: sendTerminatedEvent

-- |
--  Any errors should be send back as False result Response
--
instance StateRequestIF DebugRunState DAP.SetBreakpointsRequest where
  action (DebugRun_SetBreakpoints req) = do
    liftIO $ L.debugM _LOG_APP $ "DebugRunState SetBreakpointsRequest called. " ++ show req
    SU.setBreakpointsRequest req

-- |
--  Any errors should be send back as False result Response
--
instance StateRequestIF DebugRunState DAP.SetExceptionBreakpointsRequest where
  action (DebugRun_SetExceptionBreakpoints req) = do
    liftIO $ L.debugM _LOG_APP $ "DebugRunState SetExceptionBreakpointsRequest called. " ++ show req
    SU.setExceptionBreakpointsRequest req

-- |
--  Any errors should be send back as False result Response
--
instance StateRequestIF DebugRunState DAP.SetFunctionBreakpointsRequest where
  action (DebugRun_SetFunctionBreakpoints req) = do
    liftIO $ L.debugM _LOG_APP $ "DebugRunState SetFunctionBreakpointsRequest called. " ++ show req
    SU.setFunctionBreakpointsRequest req

