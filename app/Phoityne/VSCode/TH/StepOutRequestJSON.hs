{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.StepOutRequestJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility
import Phoityne.VSCode.TH.StepOutArgumentsJSON

-- |
--   StepOutIn request; value of command field is "stepOut".
--   The request starts the debuggee to run again for one step.
--   penDebug will respond with a StoppedEvent (event type 'step') after running the step.
--
data StepOutRequest =
  StepOutRequest {
    seqStepOutRequest       :: Int               -- Sequence number
  , typeStepOutRequest      :: String            -- One of "request", "response", or "event"
  , commandStepOutRequest   :: String            -- The command to execute
  , argumentsStepOutRequest :: StepOutArguments  -- Arguments for "stepOut" request.
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "StepOutRequest") } ''StepOutRequest)
