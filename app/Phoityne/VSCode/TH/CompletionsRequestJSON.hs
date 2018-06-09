{-# LANGUAGE TemplateHaskell #-}


module Phoityne.VSCode.TH.CompletionsRequestJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility
import Phoityne.VSCode.TH.CompletionsArgumentsJSON

-- |
--   CompletionsRequest request; value of command field is 'completions'.
--   Returns a list of possible completions for a given caret position and text.
--   The CompletionsRequest may only be called if the 'supportsCompletionsRequest' capability exists and is true.
--
data CompletionsRequest =
  CompletionsRequest {
    seqCompletionsRequest       :: Int                   -- Sequence number
  , typeCompletionsRequest      :: String                -- One of "request", "response", or "event"
  , commandCompletionsRequest   :: String                -- The command to execute
  , argumentsCompletionsRequest :: CompletionsArguments  -- Arguments for "completions" request.
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "CompletionsRequest") } ''CompletionsRequest)
