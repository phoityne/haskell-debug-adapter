{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.StackTraceArgumentsJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility

-- |
--   Arguments for "stackTrace" request.
--
data StackTraceArguments =
  StackTraceArguments {
    threadIdStackTraceArguments   :: Int        --  Retrieve the stacktrace for this thread.
  , startFrameStackTraceArguments :: Maybe Int  -- The index of the first frame to return; if omitted frames start at 0.
  , levelsStackTraceArguments     :: Int        -- The maximum number of frames to return. If levels is not specified or 0, all frames are returned.
  } deriving (Show, Read, Eq)


$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "StackTraceArguments") } ''StackTraceArguments)
