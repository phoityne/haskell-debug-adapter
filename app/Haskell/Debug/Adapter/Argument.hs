{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-cse #-}

module Haskell.Debug.Adapter.Argument (
  setting
) where

import Paths_haskell_debug_adapter (version)
import Data.Version (showVersion)
import System.Console.CmdArgs

import Haskell.Debug.Adapter.Control

-- |
--  Annotation Setting
--
setting :: ArgData
setting = modes [mode]
  &= summary summaryMsg
  &= program "haskell-debug-adapter"
         
  where
    mode = ArgData {
        _hackageVersionArgData = showVersion version
          &= name "hackage-version"
          &= typ "VERSION"
          &= explicit
          &= help "hackage module version."
      } &= name "Mode"
        &= details detailMsg
        &= auto

    summaryMsg = unlines [
        ""
      , "VERSION: haskell-debug-adapter-" ++ showVersion version
      , ""
      ]
           
    detailMsg = [
        ""
      , "  DESCRIPTION: ... "
      , ""
      , "     Please see README.md"
      , ""
      ]

