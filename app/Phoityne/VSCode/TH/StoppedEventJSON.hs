{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.StoppedEventJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility
import Phoityne.VSCode.TH.StoppedEventBodyJSON

-- |
--   Event message for "stopped" event type.
--   The event indicates that the execution of the debuggee has stopped due to some condition.
--   This can be caused by a break point previously set, a stepping action has completed, by executing a debugger statement etc.
--
data StoppedEvent =
  StoppedEvent {
    seqStoppedEvent   :: Int     -- Sequence number
  , typeStoppedEvent  :: String  -- One of "request", "response", or "event"
  , eventStoppedEvent :: String  -- Type of event
  , bodyStoppedEvent  :: StoppedEventBody
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "StoppedEvent") } ''StoppedEvent)

defaultStoppedEvent :: Int -> StoppedEvent
defaultStoppedEvent seq = StoppedEvent seq "event" "stopped" defaultStoppedEventBody

