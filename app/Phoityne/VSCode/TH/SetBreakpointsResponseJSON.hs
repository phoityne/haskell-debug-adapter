{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.SetBreakpointsResponseJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility
import Phoityne.VSCode.TH.SetBreakpointsRequestJSON
import Phoityne.VSCode.TH.SetBreakpointsResponseBodyJSON

-- |
--   Response to "setBreakpoints" request.
--   Returned is information about each breakpoint created by this request.
--   This includes the actual code location and whether the breakpoint could be verified.
--   The breakpoints returned are in the same order as the elements of the 'breakpoints'
--   (or the deprecated 'lines') in the SetBreakpointsArguments.
--
data SetBreakpointsResponse =
  SetBreakpointsResponse {
    seqSetBreakpointsResponse         :: Int     -- Sequence number
  , typeSetBreakpointsResponse        :: String  -- One of "request", "response", or "event"
  , request_seqSetBreakpointsResponse :: Int     -- Sequence number of the corresponding request
  , successSetBreakpointsResponse     :: Bool    -- Outcome of the request
  , commandSetBreakpointsResponse     :: String  -- The command requested 
  , messageSetBreakpointsResponse     :: String  -- Contains error message if success == false.
  , bodySetBreakpointsResponse        :: SetBreakpointsResponseBody
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "SetBreakpointsResponse") } ''SetBreakpointsResponse)


-- |
--
defaultSetBreakpointsResponse :: Int -> SetBreakpointsRequest ->  SetBreakpointsResponse
defaultSetBreakpointsResponse seq (SetBreakpointsRequest reqSeq _ _ _) =
  SetBreakpointsResponse seq "response" reqSeq True "setBreakpoints" "" defaultSetBreakpointsResponseBody


-- |
--
errorSetBreakpointsResponse :: Int -> SetBreakpointsRequest -> String -> SetBreakpointsResponse
errorSetBreakpointsResponse seq (SetBreakpointsRequest reqSeq _ _ _) msg =
  SetBreakpointsResponse seq "response" reqSeq False "setBreakpoints" msg defaultSetBreakpointsResponseBody

