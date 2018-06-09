{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.ScopesRequestJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility
import Phoityne.VSCode.TH.ScopesArgumentsJSON

-- |
--   Scopes request; value of command field is "scopes".
--   The request returns the variable scopes for a given stackframe ID.
--
data ScopesRequest =
  ScopesRequest {
    seqScopesRequest       :: Int              -- Sequence number
  , typeScopesRequest      :: String           -- One of "request", "response", or "event"
  , commandScopesRequest   :: String           -- The command to execute
  , argumentsScopesRequest :: ScopesArguments  -- Arguments for "scopes" request.
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "ScopesRequest") } ''ScopesRequest)
