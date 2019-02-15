{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Haskell.Debug.Adapter.State.Init.Initialize where

import Control.Monad.IO.Class
import qualified System.Log.Logger as L

import qualified GHCi.DAP as DAP
import Haskell.Debug.Adapter.Type
import Haskell.Debug.Adapter.Constant
import qualified Haskell.Debug.Adapter.Utility as U
import Haskell.Debug.Adapter.Utility

-- |
--   Any errors should be critical. don't catch anything here.
--
instance StateRequestIF InitState DAP.InitializeRequest where
  action (Init_Initialize req) = do
    liftIO $ L.debugM _LOG_APP $ "InitState InitializeRequest called. " ++ show req
    app req

-- |
--
app :: DAP.InitializeRequest -> AppContext (Maybe StateTransit)
app req = do
  debugEV _LOG_APP "initialize request called."

  resSeq <- U.getIncreasedResponseSequence
  let capa = DAP.defaultInitializeResponseCapabilities {
             DAP.supportsConfigurationDoneRequestInitializeResponseCapabilities  = True
           , DAP.supportsFunctionBreakpointsInitializeResponseCapabilities       = True
           , DAP.supportsConditionalBreakpointsInitializeResponseCapabilities    = True
           , DAP.supportsHitConditionalBreakpointsInitializeResponseCapabilities = True
           , DAP.supportsEvaluateForHoversInitializeResponseCapabilities         = True
           , DAP.exceptionBreakpointFiltersInitializeResponseCapabilities        = [
                 DAP.ExceptionBreakpointsFilter "break-on-error" "break-on-error" False
               , DAP.ExceptionBreakpointsFilter "break-on-exception" "break-on-exception" False
               ]
           , DAP.supportsStepBackInitializeResponseCapabilities                  = False
           , DAP.supportsSetVariableInitializeResponseCapabilities               = False
           , DAP.supportsRestartFrameInitializeResponseCapabilities              = False
           , DAP.supportsGotoTargetsRequestInitializeResponseCapabilities        = False
           , DAP.supportsStepInTargetsRequestInitializeResponseCapabilities      = False
           , DAP.supportsCompletionsRequestInitializeResponseCapabilities        = True
           , DAP.supportsModulesRequestInitializeResponseCapabilities            = False  -- no GUI on VSCode
           , DAP.additionalModuleColumnsInitializeResponseCapabilities           = []     -- no GUI on VSCode
           , DAP.supportsLogPointsInitializeResponseCapabilities                 = True
           , DAP.supportsTerminateRequestInitializeResponseCapabilities          = True
           }
      res  = DAP.defaultInitializeResponse {
             DAP.seqInitializeResponse         = resSeq
           , DAP.request_seqInitializeResponse = DAP.seqInitializeRequest req
           , DAP.successInitializeResponse     = True
           , DAP.bodyInitializeResponse        = capa
           }

  U.addResponse $ InitializeResponse res
  return Nothing



