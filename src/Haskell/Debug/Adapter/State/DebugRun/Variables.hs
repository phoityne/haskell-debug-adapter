{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Haskell.Debug.Adapter.State.DebugRun.Variables where

import Control.Monad.IO.Class
import qualified System.Log.Logger as L
import qualified Text.Read as R
import Control.Monad.Except

import qualified Haskell.DAP as DAP
import Haskell.Debug.Adapter.Type
import Haskell.Debug.Adapter.Constant
import qualified Haskell.Debug.Adapter.Utility as U
import qualified Haskell.Debug.Adapter.GHCi as P
import qualified Haskell.Debug.Adapter.State.Utility as SU

-- |
--  Any errors should be sent back as False result Response
--
instance StateActivityIF DebugRunStateData DAP.VariablesRequest where
  action _ (VariablesRequest req) = do
    liftIO $ L.debugM _LOG_APP $ "DebugRunState VariablesRequest called. " ++ show req
    app req

-- |
--
app :: DAP.VariablesRequest -> AppContext (Maybe StateTransit)
app req = flip catchError errHdl $ do

  let args = DAP.argumentsVariablesRequest req
      dap = ":dap-variables "
      cmd = dap ++ U.showDAP args
      dbg = dap ++ show args

  P.command cmd
  U.debugEV _LOG_APP dbg
  P.expectPmpt >>= SU.takeDapResult >>= dapHdl

  return Nothing

  where

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

