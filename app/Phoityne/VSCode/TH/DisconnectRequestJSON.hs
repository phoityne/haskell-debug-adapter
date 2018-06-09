{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.DisconnectRequestJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility
import Phoityne.VSCode.TH.DisconnectArgumentsJSON

-- |
--   Disconnect request; value of command field is "disconnect".
--
data DisconnectRequest =
  DisconnectRequest {
    seqDisconnectRequest       :: Int                        -- Sequence number
  , typeDisconnectRequest      :: String                     -- One of "request", "response", or "event"
  , commandDisconnectRequest   :: String                     -- The command to execute
  , argumentsDisconnectRequest :: Maybe DisconnectArguments  -- Arguments for "disconnect" request.
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "DisconnectRequest") } ''DisconnectRequest)

