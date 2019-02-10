{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Haskell.Debug.Adapter.State.Init.Disconnect where

import Control.Monad.IO.Class
import qualified System.Log.Logger as L

import qualified GHCi.DAP as DAP
import Haskell.Debug.Adapter.Type
import Haskell.Debug.Adapter.Constant
import qualified Haskell.Debug.Adapter.Utility as U

-- |
--   Any errors should be critical. don't catch anything here.
--
instance StateRequestIF InitState DAP.DisconnectRequest where
  action (Init_Disconnect req) = do
    liftIO $ L.debugM _LOG_APP $ "InitState DisconnectRequest called. " ++ show req
    app req


-- |
--
app :: DAP.DisconnectRequest -> AppContext (Maybe StateTransit)
app req = do
  U.sendDisconnectResponse req
  return $ Just Init_Shutdown

