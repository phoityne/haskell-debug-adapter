{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.ExitedEventJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility
import Phoityne.VSCode.TH.ExitedEventBodyJSON

-- |
--   Event message for "exited" event type.
--   The event indicates that the debuggee has exited.
--
data ExitedEvent =
  ExitedEvent {
    seqExitedEvent   :: Int     -- Sequence number
  , typeExitedEvent  :: String  -- One of "request", "response", or "event"
  , eventExitedEvent :: String  -- Type of event
  , bodyExitedEvent  :: ExitedEventBody
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "ExitedEvent") } ''ExitedEvent)

defaultExitedEvent :: Int -> Int -> ExitedEvent
defaultExitedEvent seq code = ExitedEvent seq "event" "exited" $ defaultExitedEventBody code

