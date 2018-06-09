{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.EvaluateBodyJSON where

import Data.Aeson.TH

import Phoityne.VSCode.TH.VariablePresentationHintJSON

import Phoityne.VSCode.Utility

-- |
--    Response to "evaluate" request. 
--
data EvaluateBody =
  EvaluateBody {
    resultEvaluateBody             :: String -- The result of the evaluate.
  , typeEvaluateBody               :: String -- The optional type of the evaluate result. 
  , presentationHintEvaluateBody   :: Maybe VariablePresentationHint -- Properties of a evaluate result that can be used to determine how to render the result in the UI.
  , variablesReferenceEvaluateBody :: Int       -- If variablesReference is > 0, the evaluate result is structured and its children can be retrieved by passing variablesReference to the VariablesRequest.
  , namedVariablesEvaluateBody     :: Maybe Int -- The number of named child variables. The client can use this optional information to present the variables in a paged UI and fetch them in chunks.
  , indexedVariablesEvaluateBody   :: Maybe Int -- The number of indexed child variables. The client can use this optional information to present the variables in a paged UI and fetch them in chunks.
  } deriving (Show, Read, Eq)


$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "EvaluateBody") } ''EvaluateBody)

-- |
--
defaultEvaluateBody :: EvaluateBody
defaultEvaluateBody = EvaluateBody "" "" Nothing 0 Nothing Nothing



