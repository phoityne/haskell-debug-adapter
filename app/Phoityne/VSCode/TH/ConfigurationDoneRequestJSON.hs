{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.ConfigurationDoneRequestJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility

-- |
--   ConfigurationDone request; value of command field is 'configurationDone'.
--   The client of the debug protocol must send this request at the end of the sequence of configuration requests (which was started by the InitializedEvent).
--
data ConfigurationDoneRequest =
  ConfigurationDoneRequest {
    seqConfigurationDoneRequest       :: Int               -- Sequence number
  , typeConfigurationDoneRequest      :: String            -- One of "request", "response", or "event"
  , commandConfigurationDoneRequest   :: String            -- The command to execute
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "ConfigurationDoneRequest") } ''ConfigurationDoneRequest)
