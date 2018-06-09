{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.ContinueArgumentsJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility

-- |
--   Arguments for 'continue' request.
--
data ContinueArguments =
  ContinueArguments {
    threadIdContinueArguments :: Int --  Continue execution for the specified thread (if possible). If the backend cannot continue on a single thread but will continue on all threads, it should set the allThreadsContinued attribute in the response to true.
  , exprContinueArguments     :: Maybe String -- ADD: haskell-dap
  } deriving (Show, Read, Eq)


$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "ContinueArguments") } ''ContinueArguments)
