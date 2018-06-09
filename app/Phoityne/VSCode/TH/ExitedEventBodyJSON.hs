{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.ExitedEventBodyJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility

-- |
--   Event message for "exited" event type.
--   The event indicates that the debuggee has exited.
--
data ExitedEventBody =
  ExitedEventBody {
    exitCodeExitedEventBody :: Int -- The exit code returned from the debuggee.
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "ExitedEventBody") } ''ExitedEventBody)

defaultExitedEventBody :: Int -> ExitedEventBody
defaultExitedEventBody code = ExitedEventBody code

