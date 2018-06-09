{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.ConfigurationDoneResponseJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility
import Phoityne.VSCode.TH.ConfigurationDoneRequestJSON 

-- |
--   Response to 'configurationDone' request. This is just an acknowledgement, so no body field is required.
--
data ConfigurationDoneResponse =
  ConfigurationDoneResponse {
    seqConfigurationDoneResponse         :: Int     -- Sequence number
  , typeConfigurationDoneResponse        :: String  -- One of "request", "response", or "event"
  , request_seqConfigurationDoneResponse :: Int     -- Sequence number of the corresponding request
  , successConfigurationDoneResponse     :: Bool    -- Outcome of the request
  , commandConfigurationDoneResponse     :: String  -- The command requested 
  , messageConfigurationDoneResponse     :: String  -- Contains error message if success == false.
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "ConfigurationDoneResponse") } ''ConfigurationDoneResponse)

-- |
--
defaultConfigurationDoneResponse :: Int -> ConfigurationDoneRequest ->  ConfigurationDoneResponse
defaultConfigurationDoneResponse seq (ConfigurationDoneRequest reqSeq _ _) =
  ConfigurationDoneResponse seq "response" reqSeq True "configurationDone" ""


-- |
--
parceErrorConfigurationDoneResponse :: Int -> String -> ConfigurationDoneResponse
parceErrorConfigurationDoneResponse seq msg =
  ConfigurationDoneResponse seq "response" (-1) False "configurationDone" msg

-- |
--
errorConfigurationDoneResponse :: Int -> ConfigurationDoneRequest -> String -> ConfigurationDoneResponse
errorConfigurationDoneResponse seq (ConfigurationDoneRequest reqSeq _ _) msg =
  ConfigurationDoneResponse seq "response" reqSeq False "configurationDone" msg
