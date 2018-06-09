{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.SetExceptionBreakpointsResponseJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility
import Phoityne.VSCode.TH.SetExceptionBreakpointsRequestJSON

-- |
--   Response to 'setExceptionBreakpoints' request. This is just an acknowledgement, so no body field is required.
--
data SetExceptionBreakpointsResponse =
  SetExceptionBreakpointsResponse {
    seqSetExceptionBreakpointsResponse         :: Int     -- Sequence number
  , typeSetExceptionBreakpointsResponse        :: String  -- One of "request", "response", or "event"
  , request_seqSetExceptionBreakpointsResponse :: Int     -- Sequence number of the corresponding request
  , successSetExceptionBreakpointsResponse     :: Bool    -- Outcome of the request
  , commandSetExceptionBreakpointsResponse     :: String  -- The command requested 
  , messageSetExceptionBreakpointsResponse     :: String  -- Contains error message if success == false.
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "SetExceptionBreakpointsResponse") } ''SetExceptionBreakpointsResponse)


-- |
--
defaultSetExceptionBreakpointsResponse :: Int -> SetExceptionBreakpointsRequest ->  SetExceptionBreakpointsResponse
defaultSetExceptionBreakpointsResponse seq (SetExceptionBreakpointsRequest reqSeq _ _ _) =
  SetExceptionBreakpointsResponse seq "response" reqSeq True "setExceptionBreakpoints" ""


-- |
--
errorSetExceptionBreakpointsResponse :: Int -> SetExceptionBreakpointsRequest -> String -> SetExceptionBreakpointsResponse
errorSetExceptionBreakpointsResponse seq (SetExceptionBreakpointsRequest reqSeq _ _ _) msg =
  SetExceptionBreakpointsResponse seq "response" reqSeq False "setExceptionBreakpoints" msg

