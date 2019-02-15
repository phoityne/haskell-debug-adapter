{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Haskell.Debug.Adapter.State.DebugRun.Terminate where

import Control.Monad.IO.Class
import qualified System.Log.Logger as L

import qualified GHCi.DAP as DAP
import Haskell.Debug.Adapter.Type
import Haskell.Debug.Adapter.Constant
import qualified Haskell.Debug.Adapter.Utility as U

-- |
--   Any errors should be critical. don't catch anything here.
--
instance StateRequestIF DebugRunState DAP.TerminateRequest where
  action (DebugRun_Terminate req) = do
    liftIO $ L.debugM _LOG_APP $ "DebugRunState TerminateRequest called. " ++ show req
    app req


-- |
--
app :: DAP.TerminateRequest -> AppContext (Maybe StateTransit)
app req = do
  resSeq <- U.getIncreasedResponseSequence

  let res = DAP.defaultTerminateResponse {
            DAP.seqTerminateResponse         = resSeq
          , DAP.request_seqTerminateResponse = DAP.seqTerminateRequest req
          , DAP.successTerminateResponse     = True
          }

  U.addResponse $ TerminateResponse res
  U.sendTerminateEvent
  return Nothing

