{-# LANGUAGE TemplateHaskell #-}


module Phoityne.VSCode.TH.CompletionsItemJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility

-- |
--   CompletionItems are the suggestions returned from the CompletionsRequest.
--
data CompletionsItem =
  CompletionsItem {
    labelCompletionsItem  :: String  -- The label of this completion item. By default this is also the text that is inserted when selecting this completion.
  {-
  , textCompletionsItem :: String   -- If text is not falsy then it is inserted instead of the label.
  , typeCompletionsItem :: CompletionItemType  -- The item's type. Typically the client uses this information to render the item in the UI with an icon.
  , startCompletionsItem  :: Int     -- When a completion is selected it replaces 'length' characters starting at 'start' in the text passed to the CompletionsRequest.
  , lengthCompletionsItem :: Int    --If missing the frontend will try to determine these values heuristically.
  -}
  } deriving (Show, Read, Eq)


$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "CompletionsItem") } ''CompletionsItem)
