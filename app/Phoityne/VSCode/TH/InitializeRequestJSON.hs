{-# LANGUAGE TemplateHaskell #-}

module Phoityne.VSCode.TH.InitializeRequestJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility
import Phoityne.VSCode.TH.InitializeRequestArgumentsJSON

-- |
--   Client-initiated request
--
data InitializeRequest =
  InitializeRequest {
    seqInitializeRequest       :: Int                         -- Sequence number
  , typeInitializeRequest      :: String                      -- One of "request", "response", or "event"
  , commandInitializeRequest   :: String                      -- The command to execute
  , argumentsInitializeRequest :: InitializeRequestArguments  -- Object containing arguments for the command
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "InitializeRequest") } ''InitializeRequest)

defaultInitializeRequest :: InitializeRequest
defaultInitializeRequest = InitializeRequest 0 "" "" defaultInitializeRequestArguments