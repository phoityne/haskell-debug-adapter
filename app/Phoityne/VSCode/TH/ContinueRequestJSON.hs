{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.ContinueRequestJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility
import Phoityne.VSCode.TH.ContinueArgumentsJSON

-- |
--   Continue request; value of command field is "continue".
--   The request starts the debuggee to run again.
--
data ContinueRequest =
  ContinueRequest {
    seqContinueRequest       :: Int               -- Sequence number
  , typeContinueRequest      :: String            -- One of "request", "response", or "event"
  , commandContinueRequest   :: String            -- The command to execute
  , argumentsContinueRequest :: ContinueArguments -- Arguments for "continue" request.
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "ContinueRequest") } ''ContinueRequest)
