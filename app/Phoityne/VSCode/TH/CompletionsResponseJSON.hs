{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.CompletionsResponseJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility
import Phoityne.VSCode.TH.CompletionsResponseBodyJSON
import Phoityne.VSCode.TH.CompletionsRequestJSON

-- |
--  Response to 'completions' request.
--
data CompletionsResponse =
  CompletionsResponse {
    seqCompletionsResponse         :: Int     -- Sequence number
  , typeCompletionsResponse        :: String  -- One of "request", "response", or "event"
  , request_seqCompletionsResponse :: Int     -- Sequence number of the corresponding request
  , successCompletionsResponse     :: Bool    -- Outcome of the request
  , commandCompletionsResponse     :: String  -- The command requested 
  , messageCompletionsResponse     :: String  -- Contains error message if success == false.
  , bodyCompletionsResponse        :: CompletionsResponseBody
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "CompletionsResponse") } ''CompletionsResponse)

-- |
--
defaultCompletionsResponse :: Int -> CompletionsRequest -> CompletionsResponse
defaultCompletionsResponse seq (CompletionsRequest reqSeq _ _ _) =
  CompletionsResponse seq "response" reqSeq True "completions" "" defaultCompletionsResponseBody

-- |
--
errorCompletionsResponse :: Int -> CompletionsRequest -> String -> CompletionsResponse
errorCompletionsResponse seq (CompletionsRequest reqSeq _ _ _) msg =
  CompletionsResponse seq "response" reqSeq False "completions" msg defaultCompletionsResponseBody


