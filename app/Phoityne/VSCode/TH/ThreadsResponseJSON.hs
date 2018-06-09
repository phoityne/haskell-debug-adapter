{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.ThreadsResponseJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility
import Phoityne.VSCode.TH.ThreadsResponseBodyJSON
import Phoityne.VSCode.TH.ThreadsRequestJSON

-- |
--  Response to "threads" request. 
--
data ThreadsResponse =
  ThreadsResponse {
    seqThreadsResponse         :: Int     -- Sequence number
  , typeThreadsResponse        :: String  -- One of "request", "response", or "event"
  , request_seqThreadsResponse :: Int     -- Sequence number of the corresponding request
  , successThreadsResponse     :: Bool    -- Outcome of the request
  , commandThreadsResponse     :: String  -- The command requested 
  , messageThreadsResponse     :: String  -- Contains error message if success == false.
  , bodyThreadsResponse        :: ThreadsResponseBody
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "ThreadsResponse") } ''ThreadsResponse)



-- |
--
defaultThreadsResponse :: Int -> ThreadsRequest -> ThreadsResponse
defaultThreadsResponse seq (ThreadsRequest reqSeq _ _) =
  ThreadsResponse seq "response" reqSeq True "threads" "" defaultThreadsResponseBody


-- |
--
errorThreadsResponse :: Int -> ThreadsRequest -> String -> ThreadsResponse
errorThreadsResponse seq (ThreadsRequest reqSeq _ _) msg =
  ThreadsResponse seq "response" reqSeq False "threads" msg defaultThreadsResponseBody

