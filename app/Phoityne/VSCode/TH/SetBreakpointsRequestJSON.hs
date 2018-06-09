{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.SetBreakpointsRequestJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility
import Phoityne.VSCode.TH.SetBreakpointsArgumentsJSON

-- |
--   SetBreakpoints request; value of command field is "setBreakpoints".
--   Sets multiple breakpoints for a single source and clears all previous breakpoints in that source.
--   To clear all breakpoint for a source, specify an empty array.
--   When a breakpoint is hit, a StoppedEvent (event type 'breakpoint') is generated.
--
data SetBreakpointsRequest =
  SetBreakpointsRequest {
    seqSetBreakpointsRequest       :: Int                             -- Sequence number
  , typeSetBreakpointsRequest      :: String                          -- One of "request", "response", or "event"
  , commandSetBreakpointsRequest   :: String                          -- The command to execute
  , argumentsSetBreakpointsRequest :: SetBreakpointsArguments  -- Arguments for "setBreakpoints" request.
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "SetBreakpointsRequest") } ''SetBreakpointsRequest)
