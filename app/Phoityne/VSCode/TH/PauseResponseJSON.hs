{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.PauseResponseJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility
import Phoityne.VSCode.TH.PauseRequestJSON

-- |
--  Response to "pause" request. This is just an acknowledgement, so no body field is required.
--
data PauseResponse =
  PauseResponse {
    seqPauseResponse         :: Int     -- Sequence number
  , typePauseResponse        :: String  -- One of "request", "response", or "event"
  , request_seqPauseResponse :: Int     -- Sequence number of the corresponding request
  , successPauseResponse     :: Bool    -- Outcome of the request
  , commandPauseResponse     :: String  -- The command requested 
  , messagePauseResponse     :: String  -- Contains error message if success == false.
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "PauseResponse") } ''PauseResponse)

-- |
--
defaultPauseResponse :: Int -> PauseRequest -> PauseResponse
defaultPauseResponse seq (PauseRequest reqSeq _ _ _) =
  PauseResponse seq "response" reqSeq True "pause" ""


