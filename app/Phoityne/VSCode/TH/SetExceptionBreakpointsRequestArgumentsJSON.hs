{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.SetExceptionBreakpointsRequestArgumentsJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility

-- |
--   Arguments for 'setExceptionBreakpoints' request.
--
data SetExceptionBreakpointsRequestArguments =
  SetExceptionBreakpointsRequestArguments {
    filtersSetExceptionBreakpointsRequestArguments :: [String]  -- IDs of checked exception options. The set of IDs is returned via the 'exceptionBreakpointFilters' capability.
  } deriving (Show, Read, Eq)


$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "SetExceptionBreakpointsRequestArguments") } ''SetExceptionBreakpointsRequestArguments)

