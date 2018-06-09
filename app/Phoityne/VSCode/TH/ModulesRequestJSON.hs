{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.ModulesRequestJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility
import Phoityne.VSCode.TH.ModulesArgumentsJSON

-- |
--   Modules can be retrieved from the debug adapter with the ModulesRequest which can either return all modules or a range of modules to support paging.
--
data ModulesRequest =
  ModulesRequest {
    argumentsModulesRequest :: ModulesArguments  -- command: 'modules';
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "ModulesRequest") } ''ModulesRequest)
