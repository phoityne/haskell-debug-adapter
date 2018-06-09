{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.SetExceptionBreakpointsRequestJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility
import Phoityne.VSCode.TH.SetExceptionBreakpointsRequestArgumentsJSON

-- |
--   SetExceptionBreakpoints request; value of command field is 'setExceptionBreakpoints'.
--   The request configures the debuggers response to thrown exceptions. If an exception is configured to break,
--   a StoppedEvent is fired (event type 'exception').
--
data SetExceptionBreakpointsRequest =
  SetExceptionBreakpointsRequest {
    seqSetExceptionBreakpointsRequest       :: Int                                    -- Sequence number
  , typeSetExceptionBreakpointsRequest      :: String                                 -- One of "request", "response", or "event"
  , commandSetExceptionBreakpointsRequest   :: String                                 -- The command to execute
  , argumentsSetExceptionBreakpointsRequest :: SetExceptionBreakpointsRequestArguments -- Arguments for "setExceptionBreakpoints" request.
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "SetExceptionBreakpointsRequest") } ''SetExceptionBreakpointsRequest)
