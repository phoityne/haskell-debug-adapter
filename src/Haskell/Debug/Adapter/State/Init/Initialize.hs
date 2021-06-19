{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Haskell.Debug.Adapter.State.Init.Initialize where

import Control.Monad.IO.Class
import qualified System.Log.Logger as L

import qualified Haskell.DAP as DAP
import Haskell.Debug.Adapter.Type
import Haskell.Debug.Adapter.Constant
import qualified Haskell.Debug.Adapter.Utility as U
import Haskell.Debug.Adapter.Utility



-- |
--   Any errors should be critical. don't catch anything here.
--
instance StateActivityIF InitStateData DAP.InitializeRequest where
  action _ (InitializeRequest req) = do
    liftIO $ L.debugM _LOG_APP $ "InitState InitializeRequest called. " ++ show req
    app req

-- |
--
app :: DAP.InitializeRequest -> AppContext (Maybe StateTransit)
app req = do
  debugEV _LOG_APP "initialize request called."

  resSeq <- U.getIncreasedResponseSequence
  let capa = DAP.defaultInitializeResponseBody {
             DAP.supportsConfigurationDoneRequestInitializeResponseBody  = True
           , DAP.supportsFunctionBreakpointsInitializeResponseBody       = True
           , DAP.supportsConditionalBreakpointsInitializeResponseBody    = True
           , DAP.supportsHitConditionalBreakpointsInitializeResponseBody = True
           , DAP.supportsEvaluateForHoversInitializeResponseBody         = True
           , DAP.exceptionBreakpointFiltersInitializeResponseBody        = [
             --    DAP.ExceptionBreakpointsFilter "break-on-error" "break-on-error" False
             --  , DAP.ExceptionBreakpointsFilter "break-on-exception" "break-on-exception" False
               ]
           , DAP.supportsStepBackInitializeResponseBody                  = False
           , DAP.supportsSetVariableInitializeResponseBody               = False
           , DAP.supportsRestartFrameInitializeResponseBody              = False
           , DAP.supportsGotoTargetsRequestInitializeResponseBody        = False
           , DAP.supportsStepInTargetsRequestInitializeResponseBody      = False
           , DAP.supportsCompletionsRequestInitializeResponseBody        = True
           , DAP.supportsModulesRequestInitializeResponseBody            = False  -- no GUI on VSCode
           , DAP.additionalModuleColumnsInitializeResponseBody           = []     -- no GUI on VSCode
           , DAP.supportsLogPointsInitializeResponseBody                 = True
           , DAP.supportsTerminateRequestInitializeResponseBody          = True
           }
      res  = DAP.defaultInitializeResponse {
             DAP.seqInitializeResponse         = resSeq
           , DAP.request_seqInitializeResponse = DAP.seqInitializeRequest req
           , DAP.successInitializeResponse     = True
           , DAP.bodyInitializeResponse        = capa
           }

  U.addResponse $ InitializeResponse res
  return Nothing



