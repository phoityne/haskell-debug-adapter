{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.VariablesArgumentsJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility

-- |
--   Arguments for "variables" request.
--
data VariablesArguments =
  VariablesArguments {
    variablesReferenceVariablesArguments :: Int  -- The Variable reference. 
  } deriving (Show, Read, Eq)


$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "VariablesArguments") } ''VariablesArguments)
