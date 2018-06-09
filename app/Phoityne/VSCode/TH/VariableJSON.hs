{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.VariableJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility

import Phoityne.VSCode.TH.VariablePresentationHintJSON
-- |
--   A Variable is a name/value pair.
--   If the value is structured (has children), a handle is provided to retrieve the children with the VariablesRequest.
--
data Variable =
  Variable {
    nameVariable               :: String  -- The variable's name.
  , valueVariable              :: String  -- The variable's value. This can be a multi-line text, e.g. for a function the body of a function.
  , typeVariable               :: String  -- The type of the variable's value. Typically shown in the UI when hovering over the value.
  , presentationHintVariable   :: Maybe VariablePresentationHint -- Properties of a variable that can be used to determine how to render the variable in the UI.
  , evaluateNameVariable       :: Maybe String  -- Optional evaluatable name of this variable which can be passed to the 'EvaluateRequest' to fetch the variable's value.
  , variablesReferenceVariable :: Int     -- If variablesReference is > 0, the variable is structured and its children can be retrieved by passing variablesReference to the VariablesRequest.
  , namedVariablesVariable     :: Maybe Int     -- The number of named child variables.
  , indexedVariablesVariable   :: Maybe Int -- The number of indexed child variables. The client can use this optional information to present the children in a paged UI and fetch them in chunks.
  } deriving (Show, Read, Eq)


$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "Variable") } ''Variable)
