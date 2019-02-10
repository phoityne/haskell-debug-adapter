{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Haskell.Debug.Adapter.State.Utility where

-- import Control.Monad.IO.Class
-- import qualified System.Log.Logger as L
import qualified Text.Read as R
import qualified Data.List as L
import Control.Monad.Except

import qualified GHCi.DAP as DAP
import Haskell.Debug.Adapter.Type
import Haskell.Debug.Adapter.Constant
import qualified Haskell.Debug.Adapter.Utility as U
import qualified Haskell.Debug.Adapter.GHCi as P


-- | 
--
unsupported :: String -> AppContext WrapStateRequest
unsupported reqStr = do
  let msg = "InitState does not support this request. " ++ reqStr
  throwError msg


-- |
--
setBreakpointsRequest :: DAP.SetBreakpointsRequest -> AppContext (Maybe StateTransit)
setBreakpointsRequest req = flip catchError errHdl $ do
  let args = DAP.argumentsSetBreakpointsRequest req
      cmd = ":dap-set-breakpoints " ++ U.showDAP args

  P.cmdAndOut cmd
  P.expectH $ P.funcCallBk lineCallBk

  return Nothing
  
  where
    lineCallBk :: Bool -> String -> AppContext ()
    lineCallBk True  s = U.sendStdoutEvent s
    lineCallBk False s
      | L.isPrefixOf _DAP_HEADER s = dapHdl $ drop (length _DAP_HEADER) s
      | otherwise = U.sendStdoutEventLF s

    -- |
    --
    dapHdl :: String -> AppContext ()
    dapHdl str = case R.readEither str of
      Left err -> throwError $ err ++ " : " ++ str
      Right (Left err) -> throwError $ err ++ " : " ++ str
      Right (Right body) -> do
        resSeq <- U.getIncreasedResponseSequence
        let res = DAP.defaultSetBreakpointsResponse {
                  DAP.seqSetBreakpointsResponse = resSeq
                , DAP.request_seqSetBreakpointsResponse = DAP.seqSetBreakpointsRequest req
                , DAP.successSetBreakpointsResponse = True
                , DAP.bodySetBreakpointsResponse = body
                }

        U.addResponse $ SetBreakpointsResponse res

    -- |
    --
    errHdl :: String -> AppContext (Maybe StateTransit)
    errHdl msg = do
      U.errorEV _LOG_APP msg
      resSeq <- U.getIncreasedResponseSequence
      let res = DAP.defaultSetBreakpointsResponse {
                DAP.seqSetBreakpointsResponse = resSeq
              , DAP.request_seqSetBreakpointsResponse = DAP.seqSetBreakpointsRequest req
              , DAP.successSetBreakpointsResponse = False
              , DAP.messageSetBreakpointsResponse = msg
              }

      U.addResponse $ SetBreakpointsResponse res
      return Nothing


-- |
--
setExceptionBreakpointsRequest :: DAP.SetExceptionBreakpointsRequest -> AppContext (Maybe StateTransit)
setExceptionBreakpointsRequest req = do
  let args = DAP.argumentsSetExceptionBreakpointsRequest req
      filters = DAP.filtersSetExceptionBreakpointsRequestArguments args

  mapM_ go $ getOptions filters

  resSeq <- U.getIncreasedResponseSequence
  let res = DAP.defaultSetExceptionBreakpointsResponse {
            DAP.seqSetExceptionBreakpointsResponse = resSeq
          , DAP.request_seqSetExceptionBreakpointsResponse = DAP.seqSetExceptionBreakpointsRequest req
          , DAP.successSetExceptionBreakpointsResponse = True
          }

  U.addResponse $ SetExceptionBreakpointsResponse res

  return Nothing
  
  where
    getOptions filters
      | null filters                      = ["-fno-break-on-exception", "-fno-break-on-error"]
      | filters == ["break-on-error"]     = ["-fno-break-on-exception", "-fbreak-on-error"]
      | filters == ["break-on-exception"] = ["-fbreak-on-exception",    "-fno-break-on-error"]
      | otherwise                         = ["-fbreak-on-exception",    "-fbreak-on-error" ] 

    go opt = do
      let cmd = ":set " ++ opt

      P.cmdAndOut cmd
      P.expectH $ P.stdoutCallBk



-- |
--
setFunctionBreakpointsRequest :: DAP.SetFunctionBreakpointsRequest -> AppContext (Maybe StateTransit)
setFunctionBreakpointsRequest req = flip catchError errHdl $ do
  let args = DAP.argumentsSetFunctionBreakpointsRequest req
      cmd = ":dap-set-function-breakpoints " ++ U.showDAP args

  P.cmdAndOut cmd
  P.expectH $ P.funcCallBk lineCallBk

  return Nothing
  
  where
    lineCallBk :: Bool -> String -> AppContext ()
    lineCallBk True  s = U.sendStdoutEvent s
    lineCallBk False s
      | L.isPrefixOf _DAP_HEADER s = dapHdl $ drop (length _DAP_HEADER) s
      | otherwise = U.sendStdoutEventLF s

    -- |
    --
    dapHdl :: String -> AppContext ()
    dapHdl str = case R.readEither str of
      Left err -> throwError $ err ++ " : " ++ str
      Right (Left err) -> throwError $ err ++ " : " ++ str
      Right (Right body) -> do
        resSeq <- U.getIncreasedResponseSequence
        let res = DAP.defaultSetFunctionBreakpointsResponse {
                  DAP.seqSetFunctionBreakpointsResponse = resSeq
                , DAP.request_seqSetFunctionBreakpointsResponse = DAP.seqSetFunctionBreakpointsRequest req
                , DAP.successSetFunctionBreakpointsResponse = True
                , DAP.bodySetFunctionBreakpointsResponse = body
                }

        U.addResponse $ SetFunctionBreakpointsResponse res

    -- |
    --
    errHdl :: String -> AppContext (Maybe StateTransit)
    errHdl msg = do
      U.errorEV _LOG_APP msg
      resSeq <- U.getIncreasedResponseSequence
      let res = DAP.defaultSetFunctionBreakpointsResponse {
                DAP.seqSetFunctionBreakpointsResponse = resSeq
              , DAP.request_seqSetFunctionBreakpointsResponse = DAP.seqSetFunctionBreakpointsRequest req
              , DAP.successSetFunctionBreakpointsResponse = False
              , DAP.messageSetFunctionBreakpointsResponse = msg
              }

      U.addResponse $ SetFunctionBreakpointsResponse res
      return Nothing
