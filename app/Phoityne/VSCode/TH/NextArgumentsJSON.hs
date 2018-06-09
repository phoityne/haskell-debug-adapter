{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.NextArgumentsJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility

-- |
--   Arguments for "next" request.
--
data NextArguments =
  NextArguments {
    threadIdNextArguments :: Int --  Continue execution for this thread.
  } deriving (Show, Read, Eq)


$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "NextArguments") } ''NextArguments)
