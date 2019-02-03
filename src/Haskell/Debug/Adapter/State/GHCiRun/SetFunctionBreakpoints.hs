{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Haskell.Debug.Adapter.State.GHCiRun.SetFunctionBreakpoints where

import Control.Monad.IO.Class
import qualified System.Log.Logger as L
import qualified Text.Read as R
import qualified Data.List as L

import qualified GHCi.DAP as DAP
import Haskell.Debug.Adapter.Type
import Haskell.Debug.Adapter.Constant
import qualified Haskell.Debug.Adapter.Utility as U
import qualified Haskell.Debug.Adapter.GHCi as P

instance StateRequestIF GHCiRunState DAP.SetFunctionBreakpointsRequest where
  --action :: (StateRequest s r) -> AppContext ()
  action (GHCiRun_SetFunctionBreakpoints req) = do
    liftIO $ L.debugM _LOG_APP $ "GHCiRunState SetFunctionBreakpointsRequest called. " ++ show req
    app req

-- |
--
app :: DAP.SetFunctionBreakpointsRequest -> AppContext (Maybe StateTransit)
app req = do
  
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
      Left err -> errHdl str err
      Right (Left err) -> errHdl str err
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
    errHdl :: String -> String -> AppContext()
    errHdl str err = do
      let msg = "setFunctionBreakpointRequest failed. " ++ err ++ " : " ++ str
      liftIO $ L.errorM _LOG_APP msg
      resSeq <- U.getIncreasedResponseSequence
      let res = DAP.defaultSetFunctionBreakpointsResponse {
                DAP.seqSetFunctionBreakpointsResponse = resSeq
              , DAP.request_seqSetFunctionBreakpointsResponse = DAP.seqSetFunctionBreakpointsRequest req
              , DAP.successSetFunctionBreakpointsResponse = False
              , DAP.messageSetFunctionBreakpointsResponse = msg
              }

      U.addResponse $ SetFunctionBreakpointsResponse res

