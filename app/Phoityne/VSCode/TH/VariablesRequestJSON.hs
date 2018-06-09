{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.VariablesRequestJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility
import Phoityne.VSCode.TH.VariablesArgumentsJSON

-- |
--   Variables request; value of command field is "variables".
--   Retrieves all children for the given variable reference.
--
data VariablesRequest =
  VariablesRequest {
    seqVariablesRequest       :: Int                 -- Sequence number
  , typeVariablesRequest      :: String              -- One of "request", "response", or "event"
  , commandVariablesRequest   :: String              -- The command to execute
  , argumentsVariablesRequest :: VariablesArguments  -- Arguments for "variables" request.
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "VariablesRequest") } ''VariablesRequest)
