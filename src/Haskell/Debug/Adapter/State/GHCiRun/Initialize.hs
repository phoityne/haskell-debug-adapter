{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Haskell.Debug.Adapter.State.GHCiRun.Initialize where

import Control.Monad.IO.Class
import qualified System.Log.Logger as L
import Control.Monad.Except


import qualified GHCi.DAP as DAP
import Haskell.Debug.Adapter.Type
import Haskell.Debug.Adapter.Constant

instance StateRequestIF GHCiRunState DAP.InitializeRequest where
  --action :: (StateRequest s r) -> AppContext ()
  action (GHCiRun_Initialize req) = do
    let msg = "GHCiRunState InitializeRequest is not supported." ++ show req
    liftIO $ L.errorM _LOG_APP msg
    throwError msg
