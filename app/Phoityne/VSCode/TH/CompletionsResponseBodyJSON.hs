{-# LANGUAGE TemplateHaskell #-}


module Phoityne.VSCode.TH.CompletionsResponseBodyJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility
import Phoityne.VSCode.TH.CompletionsItemJSON

-- |
--    Response to 'completions' request.
--
data CompletionsResponseBody =
  CompletionsResponseBody {
    targetsCompletionsResponseBody :: [CompletionsItem]  -- The possible completions for .
  } deriving (Show, Read, Eq)


$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "CompletionsResponseBody") } ''CompletionsResponseBody)

-- |
--
defaultCompletionsResponseBody :: CompletionsResponseBody
defaultCompletionsResponseBody = CompletionsResponseBody []

