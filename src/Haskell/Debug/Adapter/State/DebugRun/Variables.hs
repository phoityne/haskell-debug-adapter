{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Haskell.Debug.Adapter.State.DebugRun.Variables where

import Control.Monad.IO.Class
import qualified System.Log.Logger as L
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
instance StateRequestIF DebugRunState DAP.VariablesRequest where
  action (DebugRun_Variables req) = do
    liftIO $ L.debugM _LOG_APP $ "DebugRunState VariablesRequest called. " ++ show req
    app req

-- |
--
app :: DAP.VariablesRequest -> AppContext (Maybe StateTransit)
app req = flip catchError errHdl $ do
  
  let args = DAP.argumentsVariablesRequest req
      cmd = ":dap-variables " ++ U.showDAP args

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
      Left err -> errHdl err >> return ()
      Right (Left err) -> errHdl err >> return ()
      Right (Right body) -> do
        resSeq <- U.getIncreasedResponseSequence
        let res = DAP.defaultVariablesResponse {
                  DAP.seqVariablesResponse = resSeq
                , DAP.request_seqVariablesResponse = DAP.seqVariablesRequest req
                , DAP.successVariablesResponse = True
                , DAP.bodyVariablesResponse = body
                }

        U.addResponse $ VariablesResponse res

    -- |
    --
    errHdl :: String -> AppContext (Maybe StateTransit)
    errHdl msg = do
      liftIO $ L.errorM _LOG_APP msg
      resSeq <- U.getIncreasedResponseSequence
      let res = DAP.defaultVariablesResponse {
                DAP.seqVariablesResponse = resSeq
              , DAP.request_seqVariablesResponse = DAP.seqVariablesRequest req
              , DAP.successVariablesResponse = False
              , DAP.messageVariablesResponse = msg
              }

      U.addResponse $ VariablesResponse res
      return Nothing

