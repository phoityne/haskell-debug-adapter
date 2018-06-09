{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.SetFunctionBreakpointsArgumentsJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility
import Phoityne.VSCode.TH.FunctionBreakpointJSON

-- |
--   Arguments for "setFunctionBreakpoints" request.
--
data SetFunctionBreakpointsArguments =
  SetFunctionBreakpointsArguments {
    breakpointsSetFunctionBreakpointsArguments    :: [FunctionBreakpoint]  -- The function names of the breakpoints.
  } deriving (Show, Read, Eq)


$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "SetFunctionBreakpointsArguments") } ''SetFunctionBreakpointsArguments)

