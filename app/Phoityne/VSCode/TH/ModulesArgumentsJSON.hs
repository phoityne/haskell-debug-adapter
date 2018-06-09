{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.ModulesArgumentsJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility

-- |
--   Arguments for 'modules' request.
--
data ModulesArguments =
  ModulesArguments {
    startModuleModulesArguments  :: Maybe Int  -- The index of the first module to return; if omitted modules start at 0.
  , moduleCounteModulesArguments :: Maybe Int  -- The number of modules to return. If moduleCount is not specified or 0, all modules are returned.
  } deriving (Show, Read, Eq)


$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "ModulesArguments") } ''ModulesArguments)
