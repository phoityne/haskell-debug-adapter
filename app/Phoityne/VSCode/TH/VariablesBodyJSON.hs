{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.VariablesBodyJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility
import Phoityne.VSCode.TH.VariableJSON

-- |
--    Response to "variables" request. 
--
data VariablesBody =
  VariablesBody {
    variablesVariablesBody :: [Variable]  -- All children for the given variable reference.
  } deriving (Show, Read, Eq)


$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "VariablesBody") } ''VariablesBody)

-- |
--
defaultVariablesBody :: VariablesBody
defaultVariablesBody = VariablesBody []

