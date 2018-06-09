{-# LANGUAGE TemplateHaskell     #-}

module Phoityne.VSCode.TH.SourceBreakpointJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility

-- |
--   Properties of a breakpoint passed to the setBreakpoints request.
--
data SourceBreakpoint =
  SourceBreakpoint {
    lineSourceBreakpoint         :: Int           -- The source line of the breakpoint. 
  , columnSourceBreakpoint       :: Maybe Int     -- An optional source column of the breakpoint.
  , conditionSourceBreakpoint    :: Maybe String  --  An optional expression for conditional breakpoints.
  , hitConditionSourceBreakpoint :: Maybe String  -- An optional expression that controls how many hits of the breakpoint are ignored. The backend is expected to interpret the expression as needed.
  , logMessageSourceBreakpoint   :: Maybe String  -- ^If this attribute exists and is non-empty, the backend must not 'break' (stop) but log the message instead. Expressions within {} are interpolated.
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "SourceBreakpoint") } ''SourceBreakpoint)


