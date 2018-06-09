{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.NextResponseJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility
import Phoityne.VSCode.TH.NextRequestJSON

-- |
--   Response to "next" request. This is just an acknowledgement, so no body field is required.
--
data NextResponse =
  NextResponse {
    seqNextResponse         :: Int     -- Sequence number
  , typeNextResponse        :: String  -- One of "request", "response", or "event"
  , request_seqNextResponse :: Int     -- Sequence number of the corresponding request
  , successNextResponse     :: Bool    -- Outcome of the request
  , commandNextResponse     :: String  -- The command requested 
  , messageNextResponse     :: String  -- Contains error message if success == false.
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "NextResponse") } ''NextResponse)

-- |
--
defaultNextResponse :: Int -> NextRequest ->  NextResponse
defaultNextResponse seq (NextRequest reqSeq _ _ _) =
  NextResponse seq "response" reqSeq True "next" ""


-- |
--
errorNextResponse :: Int -> NextRequest -> String -> NextResponse
errorNextResponse seq (NextRequest reqSeq _ _ _) msg =
  NextResponse seq "response" reqSeq False "next" msg



