{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.VariablesResponseJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility
import Phoityne.VSCode.TH.VariablesBodyJSON
import Phoityne.VSCode.TH.VariablesRequestJSON

-- |
--  Response to "variables" request. 
--
data VariablesResponse =
  VariablesResponse {
    seqVariablesResponse         :: Int     -- Sequence number
  , typeVariablesResponse        :: String  -- One of "request", "response", or "event"
  , request_seqVariablesResponse :: Int     -- Sequence number of the corresponding request
  , successVariablesResponse     :: Bool    -- Outcome of the request
  , commandVariablesResponse     :: String  -- The command requested 
  , messageVariablesResponse     :: String  -- Contains error message if success == false.
  , bodyVariablesResponse        :: VariablesBody
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "VariablesResponse") } ''VariablesResponse)



-- |
--
defaultVariablesResponse :: Int -> VariablesRequest -> VariablesResponse
defaultVariablesResponse seq (VariablesRequest reqSeq _ _ _) =
  VariablesResponse seq "response" reqSeq True "variables" "" defaultVariablesBody

-- |
--
errorVariablesResponse :: Int -> VariablesRequest -> String -> VariablesResponse
errorVariablesResponse seq (VariablesRequest reqSeq _ _ _) msg =
  VariablesResponse seq "response" reqSeq False "variables" msg defaultVariablesBody


