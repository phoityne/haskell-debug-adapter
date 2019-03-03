{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Haskell.Debug.Adapter.State.GHCiRun.ConfigurationDone where

import Control.Monad.IO.Class
import qualified System.Log.Logger as L
import Control.Monad.State
import Control.Lens


import qualified Haskell.DAP as DAP
import Haskell.Debug.Adapter.Type
import Haskell.Debug.Adapter.Constant
import qualified Haskell.Debug.Adapter.Utility as U

-- |
--   Any errors should be critical. don't catch anything here.
--
instance StateRequestIF GHCiRunState DAP.ConfigurationDoneRequest where
  action (GHCiRun_ConfigurationDone req) = do
    liftIO $ L.debugM _LOG_APP $ "GHCiRunState ConfigurationDoneRequest called. " ++ show req
    app req

-- |
--   @see https://github.com/Microsoft/vscode/issues/4902
--   @see https://microsoft.github.io/debug-adapter-protocol/overview
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
  
  -- launch response must be sent after configuration done response.
  reqSeq <- view launchReqSeqAppStores <$> get
  resSeq <- U.getIncreasedResponseSequence
  let res = DAP.defaultLaunchResponse {
            DAP.seqLaunchResponse         = resSeq
          , DAP.request_seqLaunchResponse = reqSeq
          , DAP.successLaunchResponse     = True
          }

  U.addResponse $ LaunchResponse res

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

