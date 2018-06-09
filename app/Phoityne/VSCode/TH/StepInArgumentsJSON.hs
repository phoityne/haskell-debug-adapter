{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.StepInArgumentsJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility

-- |
--   Arguments for "stepIn" request.
--
data StepInArguments =
  StepInArguments {
    threadIdStepInArguments :: Int --  Continue execution for this thread.
  } deriving (Show, Read, Eq)


$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "StepInArguments") } ''StepInArguments)
