{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Haskell.Debug.Adapter.State.DebugRun.InternalTerminate where

import Control.Monad.IO.Class
import qualified System.Log.Logger as L
import Control.Concurrent (threadDelay)

import Haskell.Debug.Adapter.Type
import Haskell.Debug.Adapter.Constant
import qualified Haskell.Debug.Adapter.Utility as U
import qualified Haskell.Debug.Adapter.State.Utility as SU

-- |
--   Any errors should be critical. don't catch anything here.
--
instance StateRequestIF DebugRunState HdaInternalTerminateRequest where
  action (DebugRun_InternalTerminate req) = do
    liftIO $ L.debugM _LOG_APP $ "DebugRunState InternalTerminateRequest called. " ++ show req
    app req


-- |
--
app :: HdaInternalTerminateRequest -> AppContext (Maybe StateTransit)
app _ = do
  SU.terminateGHCi

  liftIO $ threadDelay _1_SEC

  U.sendTerminatedEvent
  U.sendExitedEvent

  return $ Just DebugRun_Shutdown

