module Main where

import System.Exit
import qualified Haskell.Debug.Adapter.Main as M

import qualified Haskell.Debug.Adapter.Argument as A
import qualified System.Console.CmdArgs as CMD

-- |
--  Main
--
main :: IO ()
main = CMD.cmdArgs A.setting >>= M.run >>= \case
  0 -> exitSuccess
  c -> exitWith . ExitFailure $ c


