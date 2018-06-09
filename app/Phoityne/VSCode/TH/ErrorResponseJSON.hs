{-# LANGUAGE TemplateHaskell     #-}

module Phoityne.VSCode.TH.ErrorResponseJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility
import Phoityne.VSCode.TH.ErrorResponseBodyJSON

-- |
--   On error that is whenever 'success' is false, the body can provide more details.
--
data ErrorResponse =
  ErrorResponse {
    seqErrorResponse         :: Int     -- Sequence number
  , typeErrorResponse        :: String  -- One of "request", "response", or "event"
  , request_seqErrorResponse :: Int     -- Sequence number of the corresponding request
  , successErrorResponse     :: Bool    -- Outcome of the request
  , commandErrorResponse     :: String  -- The command requested 
  , messageErrorResponse     :: String  -- Contains error message if success == false.
  , bodyErrorResponse        :: ErrorResponseBody
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "ErrorResponse") } ''ErrorResponse)

-- |
--
defaultErrorResponse :: Int -> Int -> String -> ErrorResponse
defaultErrorResponse seq reqSeq cmd =
  ErrorResponse seq "response" reqSeq False cmd "" defaultErrorResponseBody
