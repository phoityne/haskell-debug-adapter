{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.DisconnectArgumentsJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility

-- |
--   Arguments for "disconnect" request.
--
data DisconnectArguments =
  DisconnectArguments {
    restartDisconnectArguments :: Maybe Bool
  } deriving (Show, Read, Eq)


$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "DisconnectArguments") } ''DisconnectArguments)
