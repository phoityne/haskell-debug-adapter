{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.ScopesResponseJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility
import Phoityne.VSCode.TH.ScopesBodyJSON
import Phoityne.VSCode.TH.ScopesRequestJSON

-- |
--  Response to "scopes" request.
--
data ScopesResponse =
  ScopesResponse {
    seqScopesResponse         :: Int     -- Sequence number
  , typeScopesResponse        :: String  -- One of "request", "response", or "event"
  , request_seqScopesResponse :: Int     -- Sequence number of the corresponding request
  , successScopesResponse     :: Bool    -- Outcome of the request
  , commandScopesResponse     :: String  -- The command requested 
  , messageScopesResponse     :: String  -- Contains error message if success == false.
  , bodyScopesResponse        :: ScopesBody
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "ScopesResponse") } ''ScopesResponse)


-- |
--
defaultScopesResponse :: Int -> ScopesRequest -> ScopesResponse
defaultScopesResponse seq (ScopesRequest reqSeq _ _ _) =
  ScopesResponse seq "response" reqSeq True "scopes" "" defaultScopesBody


-- |
--
errorScopesResponse :: Int -> ScopesRequest -> String -> ScopesResponse
errorScopesResponse seq (ScopesRequest reqSeq _ _ _) msg =
  ScopesResponse seq "response" reqSeq False "scopes" msg defaultScopesBody

