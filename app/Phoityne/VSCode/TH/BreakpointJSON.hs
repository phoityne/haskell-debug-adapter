{-# LANGUAGE TemplateHaskell     #-}

module Phoityne.VSCode.TH.BreakpointJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility
import Phoityne.VSCode.TH.SourceJSON

-- |
--   Information about a Breakpoint created in setBreakpoints or setFunctionBreakpoints.
--
data Breakpoint =
  Breakpoint {
    idBreakpoint        :: Maybe Int -- An optional unique identifier for the breakpoint.
  , verifiedBreakpoint  :: Bool      -- If true breakpoint could be set (but not necessarily at the desired location).
  , messageBreakpoint   :: String    -- An optional message about the state of the breakpoint. This is shown to the user and can be used to explain why a breakpoint could not be verified.
  , sourceBreakpoint    :: Source    -- The source where the breakpoint is located.
  , lineBreakpoint      :: Int       -- The actual line of the breakpoint.
  , columnBreakpoint    :: Int       -- The actual column of the breakpoint.
  , endLineBreakpoint   :: Int       -- An optional end line of the actual range covered by the breakpoint.
  , endColumnBreakpoint :: Int       -- n optional end column of the actual range covered by the breakpoint. If no end line is given, then the end column is assumed to be in the start line.
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "Breakpoint") } ''Breakpoint)


