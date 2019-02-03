{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Haskell.Debug.Adapter.State.DebugRun where

import Control.Monad.IO.Class
import Control.Monad.Except
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
  getStateRequest DebugRunState (WrapRequest (InitializeRequest req)) = do
    let msg = "DebugRunState does not support this request. " ++ show req
    liftIO $ L.criticalM _LOG_APP msg
    throwError msg
  
  getStateRequest DebugRunState (WrapRequest (LaunchRequest req)) = do
    let msg = "DebugRunState does not support this request. " ++ show req
    liftIO $ L.criticalM _LOG_APP msg
    throwError msg

  getStateRequest DebugRunState (WrapRequest (DisconnectRequest req)) = do
    let msg = "DebugRunState does not support this request. " ++ show req
    liftIO $ L.criticalM _LOG_APP msg
    throwError msg

  getStateRequest DebugRunState (WrapRequest (SetBreakpointsRequest req)) = do
    let msg = "DebugRunState does not support this request. " ++ show req
    liftIO $ L.criticalM _LOG_APP msg
    throwError msg

  getStateRequest DebugRunState (WrapRequest (SetFunctionBreakpointsRequest req)) = do
    let msg = "DebugRunState does not support this request. " ++ show req
    liftIO $ L.criticalM _LOG_APP msg
    throwError msg

  getStateRequest DebugRunState (WrapRequest (SetExceptionBreakpointsRequest req)) = do
    let msg = "DebugRunState does not support this request. " ++ show req
    liftIO $ L.criticalM _LOG_APP msg
    throwError msg

  getStateRequest DebugRunState (WrapRequest (ConfigurationDoneRequest req)) = do
    let msg = "DebugRunState does not support this request. " ++ show req
    liftIO $ L.criticalM _LOG_APP msg
    throwError msg

  getStateRequest DebugRunState (WrapRequest (ThreadsRequest req)) = return . WrapStateRequest $ DebugRun_Threads req
  getStateRequest DebugRunState (WrapRequest (StackTraceRequest req)) = return . WrapStateRequest $ DebugRun_StackTrace req
  getStateRequest DebugRunState (WrapRequest (ScopesRequest req)) = return . WrapStateRequest $ DebugRun_Scopes req
  getStateRequest DebugRunState (WrapRequest (VariablesRequest req)) = return . WrapStateRequest $ DebugRun_Variables req


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
      P.expect $ P.funcCallBk lineCallBk

    
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
      Right (Right body) -> do
        resSeq <- U.getIncreasedResponseSequence
        let res = DAP.defaultStoppedEvent {
                  DAP.seqStoppedEvent = resSeq
                , DAP.bodyStoppedEvent = body
                }

        U.addResponse $ StoppedEvent res

    -- |
    --
    errHdl :: String -> String -> AppContext()
    errHdl str err = do
      let msg = "start debugging failed. " ++ err ++ " : " ++ str
      liftIO $ L.errorM _LOG_APP msg
      U.sendErrorEventLF msg

      -- TODO: sendTerminateEvent




      
