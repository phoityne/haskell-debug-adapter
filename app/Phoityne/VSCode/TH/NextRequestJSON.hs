{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.NextRequestJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility
import Phoityne.VSCode.TH.NextArgumentsJSON

-- |
--   Next request; value of command field is "next".
--   The request starts the debuggee to run again for one step.
--   penDebug will respond with a StoppedEvent (event type 'step') after running the step.
--
data NextRequest =
  NextRequest {
    seqNextRequest       :: Int            -- Sequence number
  , typeNextRequest      :: String         -- One of "request", "response", or "event"
  , commandNextRequest   :: String         -- The command to execute
  , argumentsNextRequest :: NextArguments  -- Arguments for "disconnect" request.
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "NextRequest") } ''NextRequest)
