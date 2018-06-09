{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.SourceArgumentsJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility

-- |
--   Arguments for "source" request.
--
data SourceArguments =
  SourceArguments {
    sourceReferenceSourceArguments :: Int --  The reference to the source. This is the value received in Source.reference.
  } deriving (Show, Read, Eq)


$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "SourceArguments") } ''SourceArguments)
