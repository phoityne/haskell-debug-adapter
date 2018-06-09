{-# LANGUAGE TemplateHaskell #-}


module Phoityne.VSCode.TH.InitializeResponseJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility
import Phoityne.VSCode.TH.InitializeResponseCapabilitiesJSON
import Phoityne.VSCode.TH.InitializeRequestJSON

-- |
--   Server-initiated response to client request
--
data InitializeResponse =
  InitializeResponse {
    seqInitializeResponse         :: Int     -- Sequence number
  , typeInitializeResponse        :: String  -- One of "request", "response", or "event"
  , request_seqInitializeResponse :: Int     -- Sequence number of the corresponding request
  , successInitializeResponse     :: Bool    -- Outcome of the request
  , commandInitializeResponse     :: String  -- The command requested 
  , messageInitializeResponse     :: String  -- Contains error message if success == false.
  , bodyInitializeResponse        :: InitializeResponseCapabilities  -- The capabilities of this debug adapter
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "InitializeResponse") } ''InitializeResponse)


-- |
--
parseErrorInitializeResponse :: Int -> String -> InitializeResponse
parseErrorInitializeResponse seq msg =
  InitializeResponse seq "response" seq False "initialize" msg defaultInitializeResponseCapabilities

-- |
--
errorInitializeResponse :: Int -> InitializeRequest -> String -> InitializeResponse
errorInitializeResponse seq (InitializeRequest reqSeq _ _ _) msg =
  InitializeResponse seq "response" reqSeq False "initialize" msg defaultInitializeResponseCapabilities

