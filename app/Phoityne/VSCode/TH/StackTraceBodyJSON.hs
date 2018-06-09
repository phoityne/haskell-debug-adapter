{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.StackTraceBodyJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility
import Phoityne.VSCode.TH.StackFrameJSON

-- |
--   Body for "stackTrace" request.
--
data StackTraceBody =
  StackTraceBody {
    stackFramesStackTraceBody :: [StackFrame]  -- The frames of the stackframe. If the array has length zero, there are no stackframes available. This means that there is no location information available. 
  , totalFramesStackTraceBody :: Int           -- The total number of frames available.
  } deriving (Show, Read, Eq)


$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "StackTraceBody") } ''StackTraceBody)


-- |
--
defaultStackTraceBody :: StackTraceBody
defaultStackTraceBody = StackTraceBody [] 0


