{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.ThreadsRequestJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility

-- |
--   Thread request; value of command field is "threads".
--   The request retrieves a list of all threads.
--
data ThreadsRequest =
  ThreadsRequest {
    seqThreadsRequest       :: Int              -- Sequence number
  , typeThreadsRequest      :: String           -- One of "request", "response", or "event"
  , commandThreadsRequest   :: String           -- The command to execute
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "ThreadsRequest") } ''ThreadsRequest)
