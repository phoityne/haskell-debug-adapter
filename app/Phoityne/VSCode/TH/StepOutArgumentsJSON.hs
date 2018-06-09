{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.StepOutArgumentsJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility

-- |
--   Arguments for "stepOut" request.
--
data StepOutArguments =
  StepOutArguments {
    threadIdStepOutArguments :: Int --  Continue execution for this thread.
  } deriving (Show, Read, Eq)


$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "StepOutArguments") } ''StepOutArguments)
