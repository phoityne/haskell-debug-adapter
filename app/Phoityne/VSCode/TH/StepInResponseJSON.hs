{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.StepInResponseJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility
import Phoityne.VSCode.TH.StepInRequestJSON

-- |
--  Response to "stepIn" request. This is just an acknowledgement, so no body field is required.
--
data StepInResponse =
  StepInResponse {
    seqStepInResponse         :: Int     -- Sequence number
  , typeStepInResponse        :: String  -- One of "request", "response", or "event"
  , request_seqStepInResponse :: Int     -- Sequence number of the corresponding request
  , successStepInResponse     :: Bool    -- Outcome of the request
  , commandStepInResponse     :: String  -- The command requested 
  , messageStepInResponse     :: String  -- Contains error message if success == false.
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "StepInResponse") } ''StepInResponse)

-- |
--
defaultStepInResponse :: Int -> StepInRequest -> StepInResponse
defaultStepInResponse seq (StepInRequest reqSeq _ _ _) =
  StepInResponse seq "response" reqSeq True "stepIn" ""


-- |
--
errorStepInResponse :: Int -> StepInRequest -> String -> StepInResponse
errorStepInResponse seq (StepInRequest reqSeq _ _ _) msg =
  StepInResponse seq "response" reqSeq False "stepIn" msg

