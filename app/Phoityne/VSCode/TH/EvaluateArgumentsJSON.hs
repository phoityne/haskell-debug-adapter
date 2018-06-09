{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.EvaluateArgumentsJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility

-- |
--   rguments for 'evaluate' request.
--
data EvaluateArguments =
  EvaluateArguments {
    expressionEvaluateArguments :: String     -- The expression to evaluate. 
  , frameIdEvaluateArguments    :: Maybe Int  -- Evaluate the expression in the scope of this stack frame. If not specified, the expression is evaluated in the global scope. 

  {-|
    The context in which the evaluate request is run.
    Values:
    'watch': evaluate is run in a watch.

    'repl': evaluate is run from REPL console.

    'hover': evaluate is run from a data hover.
    
    etc. 
  -}
  , contextEvaluateArguments    :: String
    } deriving (Show, Read, Eq)


$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "EvaluateArguments") } ''EvaluateArguments)
