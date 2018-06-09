{-# LANGUAGE TemplateHaskell #-}


module Phoityne.VSCode.TH.SetFunctionBreakpointsResponseBodyJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility
import Phoityne.VSCode.TH.BreakpointJSON

-- |
--
--
data SetFunctionBreakpointsResponseBody =
  SetFunctionBreakpointsResponseBody {
    breakpointsSetFunctionBreakpointsResponseBody :: [Breakpoint]  -- Information about the breakpoints. The array elements are in the same order as the elements of the 'breakpoints' (or the deprecated 'lines') in the SetFunctionBreakpointsArguments.
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "SetFunctionBreakpointsResponseBody") } ''SetFunctionBreakpointsResponseBody)

-- |
--
defaultSetFunctionBreakpointsResponseBody :: SetFunctionBreakpointsResponseBody
defaultSetFunctionBreakpointsResponseBody = SetFunctionBreakpointsResponseBody []




