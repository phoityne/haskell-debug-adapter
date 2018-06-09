{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.StepOutResponseJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility
import Phoityne.VSCode.TH.StepOutRequestJSON

-- |
--  Response to "stepOut" request. This is just an acknowledgement, so no body field is required.
--
data StepOutResponse =
  StepOutResponse {
    seqStepOutResponse         :: Int     -- Sequence number
  , typeStepOutResponse        :: String  -- One of "request", "response", or "event"
  , request_seqStepOutResponse :: Int     -- Sequence number of the corresponding request
  , successStepOutResponse     :: Bool    -- Outcome of the request
  , commandStepOutResponse     :: String  -- The command requested 
  , messageStepOutResponse     :: String  -- Contains error message if success == false.
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "StepOutResponse") } ''StepOutResponse)


-- |
--
defaultStepOutResponse :: Int -> StepOutRequest -> StepOutResponse
defaultStepOutResponse seq (StepOutRequest reqSeq _ _ _) =
  StepOutResponse seq "response" reqSeq True "stepOut" ""


-- |
--
errorStepOutResponse :: Int -> StepOutRequest -> String -> StepOutResponse
errorStepOutResponse seq (StepOutRequest reqSeq _ _ _) msg =
  StepOutResponse seq "response" reqSeq False "stepOut" msg


