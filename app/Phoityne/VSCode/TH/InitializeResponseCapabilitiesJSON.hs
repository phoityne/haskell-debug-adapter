{-# LANGUAGE TemplateHaskell #-}


module Phoityne.VSCode.TH.InitializeResponseCapabilitiesJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility
import Phoityne.VSCode.TH.ExceptionBreakpointsFilterJSON
import Phoityne.VSCode.TH.ColumnDescriptorJSON

-- |
--   Information about the capabilities of a debug adapter.
--
data InitializeResponseCapabilities =
  InitializeResponseCapabilities {
    supportsConfigurationDoneRequestInitializeResponseCapabilities  :: Bool  -- The debug adapter supports the configurationDoneRequest.
  , supportsFunctionBreakpointsInitializeResponseCapabilities       :: Bool  -- The debug adapter supports functionBreakpoints.
  , supportsConditionalBreakpointsInitializeResponseCapabilities    :: Bool  -- The debug adapter supports conditionalBreakpoints.
  , supportsHitConditionalBreakpointsInitializeResponseCapabilities :: Bool  -- The debug adapter supports breakpoints that break execution after a specified number of hits.
  , supportsEvaluateForHoversInitializeResponseCapabilities         :: Bool  -- The debug adapter supports a (side effect free) evaluate request for data hovers.
  , exceptionBreakpointFiltersInitializeResponseCapabilities        :: [ExceptionBreakpointsFilter]  -- Available filters for the setExceptionBreakpoints request.
  , supportsStepBackInitializeResponseCapabilities                  :: Bool  -- The debug adapter supports stepping back.
  , supportsSetVariableInitializeResponseCapabilities               :: Bool  -- The debug adapter supports setting a variable to a value.
  , supportsRestartFrameInitializeResponseCapabilities              :: Bool  -- The debug adapter supports restarting a frame.
  , supportsGotoTargetsRequestInitializeResponseCapabilities        :: Bool  -- The debug adapter supports the gotoTargetsRequest.
  , supportsStepInTargetsRequestInitializeResponseCapabilities      :: Bool  -- The debug adapter supports the stepInTargetsRequest. 
  , supportsCompletionsRequestInitializeResponseCapabilities        :: Bool  -- The debug adapter supports the completionsRequest.
  , supportsModulesRequestInitializeResponseCapabilities            :: Bool  -- The debug adapter supports the modules request.
  , additionalModuleColumnsInitializeResponseCapabilities           :: [ColumnDescriptor] -- The set of additional module information exposed by the debug adapter.
  , supportsLogPointsInitializeResponseCapabilities                 :: Bool  -- The debug adapter supports logpoints by interpreting the 'logMessage' attribute of the SourceBreakpoint.
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "InitializeResponseCapabilities") } ''InitializeResponseCapabilities)

-- |
--
defaultInitializeResponseCapabilities :: InitializeResponseCapabilities
defaultInitializeResponseCapabilities = InitializeResponseCapabilities False False False False False [] False False False False False False False [] False

