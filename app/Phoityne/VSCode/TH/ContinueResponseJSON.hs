{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.ContinueResponseJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility
import Phoityne.VSCode.TH.ContinueRequestJSON 

-- |
--   Response to "continue" request. This is just an acknowledgement, so no body field is required. 
--
data ContinueResponse =
  ContinueResponse {
    seqContinueResponse         :: Int     -- Sequence number
  , typeContinueResponse        :: String  -- One of "request", "response", or "event"
  , request_seqContinueResponse :: Int     -- Sequence number of the corresponding request
  , successContinueResponse     :: Bool    -- Outcome of the request
  , commandContinueResponse     :: String  -- The command requested 
  , messageContinueResponse     :: String  -- Contains error message if success == false.
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "ContinueResponse") } ''ContinueResponse)

-- |
--
defaultContinueResponse :: Int -> ContinueRequest ->  ContinueResponse
defaultContinueResponse seq (ContinueRequest reqSeq _ _ _) =
  ContinueResponse seq "response" reqSeq True "continue" ""


-- |
--
parceErrorContinueResponse :: Int -> String -> ContinueResponse
parceErrorContinueResponse seq msg =
  ContinueResponse seq "response" (-1) False "continue" msg

-- |
--
errorContinueResponse :: Int -> ContinueRequest -> String -> ContinueResponse
errorContinueResponse seq (ContinueRequest reqSeq _ _ _) msg =
  ContinueResponse seq "response" reqSeq False "continue" msg
