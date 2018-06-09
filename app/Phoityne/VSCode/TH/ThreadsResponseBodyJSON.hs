{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.ThreadsResponseBodyJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility
import Phoityne.VSCode.TH.ThreadJSON

-- |
--    Response to "threads" request.
--
data ThreadsResponseBody =
  ThreadsResponseBody {
    threadsThreadsResponseBody :: [Thread]  -- All threads.
  } deriving (Show, Read, Eq)


$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "ThreadsResponseBody") } ''ThreadsResponseBody)


-- |
--
defaultThreadsResponseBody :: ThreadsResponseBody
defaultThreadsResponseBody = ThreadsResponseBody [Thread 0 "ghci main thread"]


