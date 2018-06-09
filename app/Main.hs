{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Phoityne.VSCode.Control
import System.Exit

-- |
--
main :: IO ()
main = run >>= \case
  0 -> exitSuccess
  c -> exitWith . ExitFailure $ c

