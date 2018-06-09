{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.StepInRequestJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility
import Phoityne.VSCode.TH.StepInArgumentsJSON

-- |
--   StepIn request; value of command field is "stepIn".
--   The request starts the debuggee to run again for one step.
--   The debug adapter will respond with a StoppedEvent (event type 'step') after running the step.
--
data StepInRequest =
  StepInRequest {
    seqStepInRequest       :: Int              -- Sequence number
  , typeStepInRequest      :: String           -- One of "request", "response", or "event"
  , commandStepInRequest   :: String           -- The command to execute
  , argumentsStepInRequest :: StepInArguments  -- Arguments for "stepIn" request.
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "StepInRequest") } ''StepInRequest)
