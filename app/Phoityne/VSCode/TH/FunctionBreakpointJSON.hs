{-# LANGUAGE TemplateHaskell     #-}

module Phoityne.VSCode.TH.FunctionBreakpointJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility

-- |
--   Properties of a breakpoint passed to the setFunctionBreakpoints request.
--
data FunctionBreakpoint =
  FunctionBreakpoint {
    nameFunctionBreakpoint         :: String        -- The name of the function. 
  , conditionFunctionBreakpoint    :: Maybe String  -- An optional expression for conditional breakpoints.
  , hitConditionFunctionBreakpoint :: Maybe String  -- An optional expression that controls how many hits of the breakpoint are ignored. The backend is expected to interpret the expression as needed.
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "FunctionBreakpoint") } ''FunctionBreakpoint)


