{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.EvaluateRequestJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility
import Phoityne.VSCode.TH.EvaluateArgumentsJSON

-- |
--   Evaluate request; value of command field is "evaluate".
--   Evaluates the given expression in the context of the top most stack frame.
--   The expression has access to any variables and arguments that are in scope.
--
data EvaluateRequest =
  EvaluateRequest {
    seqEvaluateRequest       :: Int                -- Sequence number
  , typeEvaluateRequest      :: String             -- One of "request", "response", or "event"
  , commandEvaluateRequest   :: String             -- The command to execute
  , argumentsEvaluateRequest :: EvaluateArguments  -- Arguments for "evaluate" request.
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "EvaluateRequest") } ''EvaluateRequest)
