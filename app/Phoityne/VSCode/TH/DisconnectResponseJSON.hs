{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.DisconnectResponseJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility
import Phoityne.VSCode.TH.DisconnectRequestJSON

-- |
--   Response to "disconnect" request. This is just an acknowledgement, so no body field is required.
--
data DisconnectResponse =
  DisconnectResponse {
    seqDisconnectResponse         :: Int     -- Sequence number
  , typeDisconnectResponse        :: String  -- One of "request", "response", or "event"
  , request_seqDisconnectResponse :: Int     -- Sequence number of the corresponding request
  , successDisconnectResponse     :: Bool    -- Outcome of the request
  , commandDisconnectResponse     :: String  -- The command requested 
  , messageDisconnectResponse     :: String  -- Contains error message if success == false.
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "DisconnectResponse") } ''DisconnectResponse)

-- |
--
defaultDisconnectResponse :: Int -> DisconnectRequest ->  DisconnectResponse
defaultDisconnectResponse seq (DisconnectRequest reqSeq _ _ _) =
  DisconnectResponse seq "response" reqSeq True "disconnect" ""

-- |
--
parceErrorDisconnectResponse :: Int -> String -> DisconnectResponse
parceErrorDisconnectResponse seq msg =
  DisconnectResponse seq "response" (-1) False "disconnect" msg

-- |
--
errorDisconnectResponse :: Int -> DisconnectRequest -> String -> DisconnectResponse
errorDisconnectResponse seq (DisconnectRequest reqSeq _ _ _) msg =
  DisconnectResponse seq "response" reqSeq False "disconnect" msg
