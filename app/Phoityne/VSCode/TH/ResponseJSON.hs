{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.ResponseJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility

-- |
--  Response to a request.
--
data Response =
  Response {
    seqResponse         :: Int     -- Sequence number. 
  , typeResponse        :: String  -- Message type. Values: 'request', 'response', 'event', etc.
  , request_seqResponse :: Int     -- Sequence number of the corresponding request.
  , successResponse     :: Bool    -- Outcome of the request. 
  , commandResponse     :: String  -- The command requested. 
  , messageResponse     :: String  -- Contains error message if success == false.
  -- , bodyResponse        :: Maybe Objecy -- Contains request result if success is true and optional error details if success is false.
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "Response") } ''Response)


-- |
--
defaultResponse :: Int -> Int -> String -> Response
defaultResponse seq reqSeq cmd =
  Response seq "response" reqSeq True cmd ""


-- |
--
errorResponse :: Int -> Int -> String -> String -> Response
errorResponse seq reqSeq cmd msg =
  Response seq "response" reqSeq False cmd msg

