{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Haskell.Debug.Adapter.State.GHCiRun.ConfigurationDone where

import Control.Monad.IO.Class
import qualified System.Log.Logger as L


import qualified GHCi.DAP as DAP
import Haskell.Debug.Adapter.Type
import Haskell.Debug.Adapter.Constant
import qualified Haskell.Debug.Adapter.Utility as U

instance StateRequestIF GHCiRunState DAP.ConfigurationDoneRequest where
  --action :: (StateRequest s r) -> AppContext ()
  action (GHCiRun_ConfigurationDone req) = do
    liftIO $ L.debugM _LOG_APP $ "GHCiRunState ConfigurationDoneRequest called. " ++ show req
    app req

-- |
--
app :: DAP.ConfigurationDoneRequest -> AppContext (Maybe StateTransit)
app req = do

  -- U.sendConsoleEvent _DEBUG_START_MSG
  -- U.sendStdoutEvent _GHCI_PROMPT_HDA

  resSeq <- U.getIncreasedResponseSequence
  let res = DAP.defaultConfigurationDoneResponse {
            DAP.seqConfigurationDoneResponse = resSeq
          , DAP.request_seqConfigurationDoneResponse = DAP.seqConfigurationDoneRequest req
          , DAP.successConfigurationDoneResponse = True
          }

  U.addResponse $ ConfigurationDoneResponse res
  
  return $ Just GHCiRun_DebugRun

{-
-- |
--
--
_DEBUG_START_MSG :: String
_DEBUG_START_MSG = L.intercalate "\n" [
    ""
  , "  Now, ghci launched and configuration done."
  , "  Press F5 to start debugging."
  , "  Or modify source code. it will be loaded to ghci automatically."
  , " "
  ]
-}

