{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Haskell.Debug.Adapter.State.DebugRun.Threads where

import Control.Monad.IO.Class
import qualified System.Log.Logger as L

import qualified Haskell.DAP as DAP
import Haskell.Debug.Adapter.Type
import Haskell.Debug.Adapter.Constant
import qualified Haskell.Debug.Adapter.Utility as U

-- |
--  Any errors should be sent back as False result Response
--
instance StateActivityIF DebugRunStateData DAP.ThreadsRequest where
  action _ (ThreadsRequest req) = do
    liftIO $ L.debugM _LOG_APP $ "DebugRunState ThreadsRequest called. " ++ show req
    app req

-- |
--
app :: DAP.ThreadsRequest -> AppContext (Maybe StateTransit)
app req = do
  resSeq <- U.getIncreasedResponseSequence
  let res = DAP.defaultThreadsResponse {
            DAP.seqThreadsResponse = resSeq
          , DAP.request_seqThreadsResponse = DAP.seqThreadsRequest req
          , DAP.successThreadsResponse = True
          }

  U.addResponse $ ThreadsResponse res

  return Nothing
