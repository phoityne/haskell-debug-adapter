{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.ScopesArgumentsJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility

-- |
--   Arguments for "scopes" request.
--
data ScopesArguments =
  ScopesArguments {
    frameIdScopesArguments :: Int  -- Retrieve the scopes for this stackframe.
  } deriving (Show, Read, Eq)


$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "ScopesArguments") } ''ScopesArguments)
