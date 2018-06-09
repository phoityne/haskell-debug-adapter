{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.SetFunctionBreakpointsResponseJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility
import Phoityne.VSCode.TH.SetFunctionBreakpointsRequestJSON
import Phoityne.VSCode.TH.SetFunctionBreakpointsResponseBodyJSON

-- |
--   Response to "setFunctionBreakpoints" request.
--
data SetFunctionBreakpointsResponse =
  SetFunctionBreakpointsResponse {
    seqSetFunctionBreakpointsResponse         :: Int     -- Sequence number
  , typeSetFunctionBreakpointsResponse        :: String  -- One of "request", "response", or "event"
  , request_seqSetFunctionBreakpointsResponse :: Int     -- Sequence number of the corresponding request
  , successSetFunctionBreakpointsResponse     :: Bool    -- Outcome of the request
  , commandSetFunctionBreakpointsResponse     :: String  -- The command requested 
  , messageSetFunctionBreakpointsResponse     :: String  -- Contains error message if success == false.
  , bodySetFunctionBreakpointsResponse        :: SetFunctionBreakpointsResponseBody
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "SetFunctionBreakpointsResponse") } ''SetFunctionBreakpointsResponse)


-- |
--
defaultSetFunctionBreakpointsResponse :: Int -> SetFunctionBreakpointsRequest ->  SetFunctionBreakpointsResponse
defaultSetFunctionBreakpointsResponse seq (SetFunctionBreakpointsRequest reqSeq _ _ _) =
  SetFunctionBreakpointsResponse seq "response" reqSeq True "setFunctionBreakpoints" "" defaultSetFunctionBreakpointsResponseBody


-- |
--
errorSetFunctionBreakpointsResponse :: Int -> SetFunctionBreakpointsRequest -> String -> SetFunctionBreakpointsResponse
errorSetFunctionBreakpointsResponse seq (SetFunctionBreakpointsRequest reqSeq _ _ _) msg =
  SetFunctionBreakpointsResponse seq "response" reqSeq False "setFunctionBreakpoints" msg defaultSetFunctionBreakpointsResponseBody

