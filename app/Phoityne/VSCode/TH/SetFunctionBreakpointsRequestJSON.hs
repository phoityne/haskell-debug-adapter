{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.SetFunctionBreakpointsRequestJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility
import Phoityne.VSCode.TH.SetFunctionBreakpointsArgumentsJSON

-- |
--   SetFunctionBreakpoints request; value of command field is "setFunctionBreakpoints".
--   Sets multiple function breakpoints and clears all previous function breakpoints.
--   To clear all function breakpoint, specify an empty array.
--   When a function breakpoint is hit, a StoppedEvent (event type 'function breakpoint') is generated.
--
data SetFunctionBreakpointsRequest =
  SetFunctionBreakpointsRequest {
    seqSetFunctionBreakpointsRequest       :: Int                                    -- Sequence number
  , typeSetFunctionBreakpointsRequest      :: String                                 -- One of "request", "response", or "event"
  , commandSetFunctionBreakpointsRequest   :: String                                 -- The command to execute
  , argumentsSetFunctionBreakpointsRequest :: SetFunctionBreakpointsArguments -- Arguments for "setFunctionBreakpoints" request.
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "SetFunctionBreakpointsRequest") } ''SetFunctionBreakpointsRequest)
