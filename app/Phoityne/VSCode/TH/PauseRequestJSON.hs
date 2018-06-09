{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.PauseRequestJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility
import Phoityne.VSCode.TH.PauseArgumentsJSON

-- |
--   Pause request; value of command field is "pause".
--   The request suspenses the debuggee.
--   penDebug will respond with a StoppedEvent (event type 'pause') after a successful 'pause' command.
--
data PauseRequest =
  PauseRequest {
    seqPauseRequest       :: Int             -- Sequence number
  , typePauseRequest      :: String          -- One of "request", "response", or "event"
  , commandPauseRequest   :: String          -- The command to execute
  , argumentsPauseRequest :: PauseArguments  -- Arguments for "pause" request.
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "PauseRequest") } ''PauseRequest)
