{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.ThreadJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility

-- |
--   A Thread is a name/value pair.
--   If the value is structured (has children), a handle is provided to retrieve the children with the ThreadsRequest.
--
data Thread =
  Thread {
    idThread   :: Int     -- Unique identifier for the thread. 
  , nameThread :: String  -- A name of the thread. 
  } deriving (Show, Read, Eq)


$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "Thread") } ''Thread)
