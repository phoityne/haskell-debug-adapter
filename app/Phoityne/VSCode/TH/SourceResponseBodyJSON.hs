{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.SourceResponseBodyJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility


-- |
--    Response to "source" request. 
--
data SourceResponseBody =
  SourceResponseBody {
    contentSourceResponseBody :: String  -- Content of the source reference
  } deriving (Show, Read, Eq)


$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "SourceResponseBody") } ''SourceResponseBody)


-- |
--
defaultSourceResponseBody :: SourceResponseBody
defaultSourceResponseBody = SourceResponseBody ""


