{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.EvaluateResponseJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility
import Phoityne.VSCode.TH.EvaluateBodyJSON
import Phoityne.VSCode.TH.EvaluateRequestJSON

-- |
--  Response to "evaluate" request.
--
data EvaluateResponse =
  EvaluateResponse {
    seqEvaluateResponse         :: Int     -- Sequence number
  , typeEvaluateResponse        :: String  -- One of "request", "response", or "event"
  , request_seqEvaluateResponse :: Int     -- Sequence number of the corresponding request
  , successEvaluateResponse     :: Bool    -- Outcome of the request
  , commandEvaluateResponse     :: String  -- The command requested 
  , messageEvaluateResponse     :: String  -- Contains error message if success == false.
  , bodyEvaluateResponse        :: EvaluateBody
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "EvaluateResponse") } ''EvaluateResponse)


-- |
--
defaultEvaluateResponse :: Int -> EvaluateRequest -> EvaluateResponse
defaultEvaluateResponse seq (EvaluateRequest reqSeq _ _ _) =
  EvaluateResponse seq "response" reqSeq True "evaluate" "" defaultEvaluateBody

-- |
--
errorEvaluateResponse :: Int -> EvaluateRequest -> String -> EvaluateResponse
errorEvaluateResponse seq (EvaluateRequest reqSeq _ _ _) msg =
  EvaluateResponse seq "response" reqSeq False "evaluate" msg defaultEvaluateBody

