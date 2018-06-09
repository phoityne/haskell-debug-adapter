{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.StackTraceRequestJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility
import Phoityne.VSCode.TH.StackTraceArgumentsJSON

-- |
--   StackTrace request; value of command field is "stackTrace".
--   The request returns a stacktrace from the current execution state.
--
data StackTraceRequest =
  StackTraceRequest {
    seqStackTraceRequest       :: Int                  -- Sequence number
  , typeStackTraceRequest      :: String               -- One of "request", "response", or "event"
  , commandStackTraceRequest   :: String               -- The command to execute
  , argumentsStackTraceRequest :: StackTraceArguments  -- Arguments for "stackTrace" request.
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "StackTraceRequest") } ''StackTraceRequest)
