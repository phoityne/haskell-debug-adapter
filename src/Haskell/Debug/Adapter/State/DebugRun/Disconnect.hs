{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Haskell.Debug.Adapter.State.DebugRun.Disconnect where

import Control.Monad.IO.Class
import qualified System.Log.Logger as L

import qualified GHCi.DAP as DAP
import Haskell.Debug.Adapter.Type
import Haskell.Debug.Adapter.Constant
import qualified Haskell.Debug.Adapter.Utility as U
import qualified Haskell.Debug.Adapter.GHCi as P

-- |
--   Any errors should be critical. don't catch anything here.
--
instance StateRequestIF DebugRunState DAP.DisconnectRequest where
  action (DebugRun_Disconnect req) = do
    liftIO $ L.debugM _LOG_APP $ "DebugRunState DisconnectRequest called. " ++ show req
    app req


-- |
--
app :: DAP.DisconnectRequest -> AppContext (Maybe StateTransit)
app req = do
  let cmd = ":quit"

  P.cmdAndOut cmd
  P.expectH $ P.stdoutCallBk
  
  U.sendDisconnectResponse req

  return $ Just DebugRun_Shutdown

