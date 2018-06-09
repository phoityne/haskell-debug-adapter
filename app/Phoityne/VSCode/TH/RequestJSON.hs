{-# LANGUAGE TemplateHaskell #-}

module Phoityne.VSCode.TH.RequestJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility

-- |
--   Client-initiated request
--
data Request =
  Request {
    seqRequest       :: Int     -- Sequence number. 
  , typeRequest      :: String  -- Message type. Values: 'request', 'response', 'event', etc.
  , commandRequest   :: String    -- The command to execute
  -- , argumentsRequest :: [String]  -- Object containing arguments for the command
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "Request") } ''Request)
