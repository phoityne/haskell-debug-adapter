{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.CompletionsArgumentsJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility

-- |
--   Arguments for 'completions' request. 
--
data CompletionsArguments =
  CompletionsArguments {
    frameIdCompletionsArguments :: Maybe Int  -- Returns completions in the scope of this stack frame. If not specified, the completions are returned for the global scope. 
  , textCompletionsArguments :: String  -- One or more source lines. Typically this is the text a user has typed into the debug console before he asked for completion.
  , columnCompletionsArguments :: Int   -- The character position for which to determine the completion proposals. 
  , lineCompletionsArguments :: Maybe Int     -- An optional line for which to determine the completion proposals. If missing the first line of the text is assumed.
  } deriving (Show, Read, Eq)


$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "CompletionsArguments") } ''CompletionsArguments)
