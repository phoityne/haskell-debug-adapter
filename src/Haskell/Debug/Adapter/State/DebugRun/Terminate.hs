{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Haskell.Debug.Adapter.State.DebugRun.Terminate where

import Control.Monad.IO.Class
import qualified System.Log.Logger as L
import Control.Concurrent (threadDelay)

import qualified Haskell.DAP as DAP
import Haskell.Debug.Adapter.Type
import Haskell.Debug.Adapter.Constant
import qualified Haskell.Debug.Adapter.Utility as U
import qualified Haskell.Debug.Adapter.State.Utility as SU

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
  SU.terminateGHCi

  liftIO $ threadDelay _1_SEC

  resSeq <- U.getIncreasedResponseSequence

  let res = DAP.defaultTerminateResponse {
            DAP.seqTerminateResponse         = resSeq
          , DAP.request_seqTerminateResponse = DAP.seqTerminateRequest req
          , DAP.successTerminateResponse     = True
          }

  U.addResponse $ TerminateResponse res
  U.sendTerminatedEvent
  U.sendExitedEvent

  return $ Just DebugRun_Shutdown

