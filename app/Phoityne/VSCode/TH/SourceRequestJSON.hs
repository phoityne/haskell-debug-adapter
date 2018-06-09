{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.SourceRequestJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility
import Phoityne.VSCode.TH.SourceArgumentsJSON

-- |
--   Source request; value of command field is "source".
--   The request retrieves the source code for a given source reference.
--
data SourceRequest =
  SourceRequest {
    seqSourceRequest       :: Int              -- Sequence number
  , typeSourceRequest      :: String           -- One of "request", "response", or "event"
  , commandSourceRequest   :: String           -- The command to execute
  , argumentsSourceRequest :: SourceArguments  -- Arguments for "source" request.
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "SourceRequest") } ''SourceRequest)
