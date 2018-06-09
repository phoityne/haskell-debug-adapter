{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.ModulesResponseJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility
import Phoityne.VSCode.TH.ModulesResponseBodyJSON

-- |
--   Response to 'modules' request.
--
data ModulesResponse =
  ModulesResponse {
    bodyModulesResponse :: ModulesResponseBody
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "ModulesResponse") } ''ModulesResponse)





