{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.SetBreakpointsArgumentsJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility
import Phoityne.VSCode.TH.SourceJSON
import Phoityne.VSCode.TH.SourceBreakpointJSON

-- |
--   Arguments for "setBreakpoints" request.
--
data SetBreakpointsArguments =
  SetBreakpointsArguments {
    sourceSetBreakpointsArguments         :: Source              --  The source location of the breakpoints; either source.path or source.reference must be specified.
  , breakpointsSetBreakpointsArguments    :: [SourceBreakpoint]  -- The code locations of the breakpoints.
  --, sourceModifiedSetBreakpointsArguments :: Bool                -- A value of true indicates that the underlying source has been modified which results in new breakpoint locations.
  } deriving (Show, Read, Eq)


$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "SetBreakpointsArguments") } ''SetBreakpointsArguments)

