{-# LANGUAGE TemplateHaskell #-}


module Phoityne.VSCode.TH.ExceptionBreakpointsFilterJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility

-- |
--   An ExceptionBreakpointsFilter is shown in the UI as an option for configuring how exceptions are dealt with.
--
data ExceptionBreakpointsFilter =
  ExceptionBreakpointsFilter {
    filterExceptionBreakpointsFilter  :: String  -- The internal ID of the filter. This value is passed to the setExceptionBreakpoints request.
  , labelExceptionBreakpointsFilter   :: String  -- The name of the filter. This will be shown in the UI.
  , defaultExceptionBreakpointsFilter :: Bool    -- Initial value of the filter. If not specified a value 'false' is assumed.
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "ExceptionBreakpointsFilter") } ''ExceptionBreakpointsFilter)

