{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.SetBreakpointsResponseBodyJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility
import Phoityne.VSCode.TH.BreakpointJSON

-- |
--   Response to "setBreakpoints" request.
--   Returned is information about each breakpoint created by this request.
--   This includes the actual code location and whether the breakpoint could be verified.
--   The breakpoints returned are in the same order as the elements of the 'breakpoints'
--   (or the deprecated 'lines') in the SetBreakpointsArguments.
--
data SetBreakpointsResponseBody =
  SetBreakpointsResponseBody {
    breakpointsSetBreakpointsResponseBody :: [Breakpoint]  -- Information about the breakpoints. The array elements are in the same order as the elements of the 'breakpoints' (or the deprecated 'lines') in the SetBreakpointsArguments.
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "SetBreakpointsResponseBody") } ''SetBreakpointsResponseBody)

-- |
--
defaultSetBreakpointsResponseBody :: SetBreakpointsResponseBody
defaultSetBreakpointsResponseBody = SetBreakpointsResponseBody []




