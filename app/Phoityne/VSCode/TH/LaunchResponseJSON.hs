{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.LaunchResponseJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility
import Phoityne.VSCode.TH.LaunchRequestJSON

-- |
--   Server-initiated response to client request
--
data LaunchResponse =
  LaunchResponse {
    seqLaunchResponse         :: Int     -- Sequence number
  , typeLaunchResponse        :: String  -- One of "request", "response", or "event"
  , request_seqLaunchResponse :: Int     -- Sequence number of the corresponding request
  , successLaunchResponse     :: Bool    -- Outcome of the request
  , commandLaunchResponse     :: String  -- The command requested 
  , messageLaunchResponse     :: String  -- Contains error message if success == false.
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "LaunchResponse") } ''LaunchResponse)


-- |
--
defaultLaunchResponse :: Int -> LaunchRequest ->  LaunchResponse
defaultLaunchResponse seq (LaunchRequest reqSeq _ _ _) =
  LaunchResponse seq "response" reqSeq True "launch" ""

-- |
--
parceErrorLaunchResponse :: Int -> String -> LaunchResponse
parceErrorLaunchResponse seq msg =
  LaunchResponse seq "response" seq False "launch" msg

-- |
--
errorLaunchResponse :: Int -> LaunchRequest -> String -> LaunchResponse
errorLaunchResponse seq (LaunchRequest reqSeq _ _ _) msg =
  LaunchResponse seq "response" reqSeq False "launch" msg



