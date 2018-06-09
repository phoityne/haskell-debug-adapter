{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.InitializedEventJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility

-- |
--   Server-initiated response to client request
--
data InitializedEvent =
  InitializedEvent {
    seqInitializedEvent   :: Int     -- Sequence number
  , typeInitializedEvent  :: String  -- One of "request", "response", or "event"
  , eventInitializedEvent :: String  -- Type of event
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "InitializedEvent") } ''InitializedEvent)

defaultInitializedEvent :: Int -> InitializedEvent
defaultInitializedEvent resSeq = InitializedEvent resSeq "event" "initialized"