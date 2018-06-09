{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.PauseArgumentsJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility

-- |
--   Arguments for "pause" request.
--
data PauseArguments =
  PauseArguments {
    threadIdPauseArguments :: Int --  Continue execution for this thread.
  } deriving (Show, Read, Eq)


$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "PauseArguments") } ''PauseArguments)
