{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Haskell.Debug.Adapter.State.DebugRun.StepIn where

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
instance StateActivityIF DebugRunStateData DAP.StepInRequest where
  action _ (StepInRequest req) = do
    liftIO $ L.debugM _LOG_APP $ "DebugRunState StepInRequest called. " ++ show req
    app req

-- |
--
app :: DAP.StepInRequest -> AppContext (Maybe StateTransit)
app req = flip catchError errHdl $ do

  let args = DAP.argumentsStepInRequest req
      dap = ":dap-step-in "
      cmd = dap ++ U.showDAP args
      dbg = dap ++ show args

  P.command cmd
  U.debugEV _LOG_APP dbg
  outStr <- P.expectPmpt

  resSeq <- U.getIncreasedResponseSequence
  let res = DAP.defaultStepInResponse {
            DAP.seqStepInResponse = resSeq
          , DAP.request_seqStepInResponse = DAP.seqStepInRequest req
          , DAP.successStepInResponse = True
          }

  U.addResponse $ StepInResponse res

  SU.takeDapResult outStr >>= dapHdl

  return Nothing

  where
    -- |
    --
    dapHdl :: String -> AppContext ()
    dapHdl str = case R.readEither str of
      Left err -> errHdl err >> return ()
      Right (Left err) -> errHdl err >> return ()
      Right (Right body) -> U.handleStoppedEventBody body


    -- |
    --
    errHdl :: String -> AppContext (Maybe StateTransit)
    errHdl msg = do
      liftIO $ L.errorM _LOG_APP msg
      resSeq <- U.getIncreasedResponseSequence
      let res = DAP.defaultStepInResponse {
                DAP.seqStepInResponse = resSeq
              , DAP.request_seqStepInResponse = DAP.seqStepInRequest req
              , DAP.successStepInResponse = False
              , DAP.messageStepInResponse = msg
              }

      U.addResponse $ StepInResponse res
      return Nothing

