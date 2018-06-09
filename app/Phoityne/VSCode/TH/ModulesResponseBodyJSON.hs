{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.ModulesResponseBodyJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility
import Phoityne.VSCode.TH.ModuleJSON

-- |
--   Response to "scopes" request.
--
data ModulesResponseBody =
  ModulesResponseBody {
    modulesModulesResponseBody :: [Module]  -- All modules or range of modules.
  , totalModulesModulesResponseBody :: Int  -- The total number of modules available.
  } deriving (Show, Read, Eq)


$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "ModulesResponseBody") } ''ModulesResponseBody)


