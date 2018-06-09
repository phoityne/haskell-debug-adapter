{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.ScopeJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility

-- |
--   A Scope is a named container for variables.
--
data Scope =
  Scope {
    nameScope               :: String     -- Name of the scope such as 'Arguments', 'Locals'. 
  , variablesReferenceScope :: Int        -- The variables of this scope can be retrieved by passing the value of variablesReference to the VariablesRequest.
  , namedVariablesScope     :: Maybe Int  -- The number of named variables in this scope. The client can use this optional information to present the variables in a paged UI and fetch them in chunks.
  , indexedVariablesScope   :: Maybe Int  -- The number of indexed variables in this scope. The client can use this optional information to present the variables in a paged UI and fetch them in chunks.
  , expensiveScope          :: Bool       -- If true, the number of variables in this scope is large or expensive to retrieve.
  } deriving (Show, Read, Eq)


$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "Scope") } ''Scope)
