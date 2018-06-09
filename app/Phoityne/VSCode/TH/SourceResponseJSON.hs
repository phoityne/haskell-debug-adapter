{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.SourceResponseJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility
import Phoityne.VSCode.TH.SourceResponseBodyJSON
import Phoityne.VSCode.TH.SourceRequestJSON

-- |
--  Response to "source" request. 
--
data SourceResponse =
  SourceResponse {
    seqSourceResponse         :: Int     -- Sequence number
  , typeSourceResponse        :: String  -- One of "request", "response", or "event"
  , request_seqSourceResponse :: Int     -- Sequence number of the corresponding request
  , successSourceResponse     :: Bool    -- Outcome of the request
  , commandSourceResponse     :: String  -- The command requested 
  , messageSourceResponse     :: String  -- Contains error message if success == false.
  , bodySourceResponse        :: SourceResponseBody
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "SourceResponse") } ''SourceResponse)



-- |
--
defaultSourceResponse :: Int -> SourceRequest -> SourceResponse
defaultSourceResponse seq (SourceRequest reqSeq _ _ _) =
  SourceResponse seq "response" reqSeq True "source" "" defaultSourceResponseBody


