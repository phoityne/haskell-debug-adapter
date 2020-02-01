module Main where

import System.Exit
import System.IO
import Control.Exception.Safe
import Data.Default

import Haskell.Debug.Adapter.Control

-- |
--  Main
--
main :: IO ()
main = flip catchAny exception
     $ flip finally  finalize
     $ run def stdin stdout

  where
    finalize = return ()
    exception e = print e >> exitFailure

