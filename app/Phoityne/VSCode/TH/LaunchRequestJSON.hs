{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.LaunchRequestJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility
import Phoityne.VSCode.TH.LaunchRequestArgumentsJSON

-- |
--   Launch request; value of command field is "launch".
--
data LaunchRequest =
  LaunchRequest {
    seqLaunchRequest       :: Int                     -- Sequence number
  , typeLaunchRequest      :: String                  -- One of "request", "response", or "event"
  , commandLaunchRequest   :: String                  -- The command to execute
  , argumentsLaunchRequest :: LaunchRequestArguments  -- Arguments for "launch" request.
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "LaunchRequest") } ''LaunchRequest)

