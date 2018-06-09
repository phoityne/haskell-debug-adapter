{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.StackTraceResponseJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility
import Phoityne.VSCode.TH.StackTraceBodyJSON
import Phoityne.VSCode.TH.StackTraceRequestJSON

-- |
--  Response to "stackTrace" request.
--
data StackTraceResponse =
  StackTraceResponse {
    seqStackTraceResponse         :: Int     -- Sequence number
  , typeStackTraceResponse        :: String  -- One of "request", "response", or "event"
  , request_seqStackTraceResponse :: Int     -- Sequence number of the corresponding request
  , successStackTraceResponse     :: Bool    -- Outcome of the request
  , commandStackTraceResponse     :: String  -- The command requested 
  , messageStackTraceResponse     :: String  -- Contains error message if success == false.
  , bodyStackTraceResponse        :: StackTraceBody
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "StackTraceResponse") } ''StackTraceResponse)

-- |
--
defaultStackTraceResponse :: Int -> StackTraceRequest -> StackTraceResponse
defaultStackTraceResponse seq (StackTraceRequest reqSeq _ _ _) =
  StackTraceResponse seq "response" reqSeq True "stackTrace" "" defaultStackTraceBody


-- |
--
errorStackTraceResponse :: Int -> StackTraceRequest -> String -> StackTraceResponse
errorStackTraceResponse seq (StackTraceRequest reqSeq _ _ _) msg =
  StackTraceResponse seq "response" reqSeq False "stackTrace" msg defaultStackTraceBody

