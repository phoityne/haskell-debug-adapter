{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# OPTIONS_GHC -fno-cse         #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Phoityne.VSCode.Argument (
  HelpExitException(..)
, ArgData(..)
, getArgData
) where

import Paths_phoityne_vscode (version)
import Phoityne.VSCode.Type
import System.Console.CmdArgs
import qualified Control.Exception.Safe as E
import qualified System.Console.CmdArgs as CMD
import Data.Version


-- |
-- 
getArgData :: IO ArgData
getArgData = E.catches (CMD.cmdArgs config) handlers
  where
    handlers = [E.Handler someExcept]
    someExcept (e :: E.SomeException) = if
      | "ExitSuccess" == show e -> E.throw HelpExitException
      | otherwise -> E.throwIO e


-- |
--
config :: ArgData
config = modes [confA]
         &= summary sumMsg
         &= program "phoityne-vscode"
         
  where
    confA = ArgData {
            _hackageVersionArgData = showVersion version
            &= name "hackage-version"
            &= typ "VERSION"
            &= explicit
            &= help "hackage module version."
          } &= name "ModeA"
            &= details mdAMsg
            &= auto

    sumMsg = unlines [
             "phoityne-vscode-" ++ showVersion version
           ]
           
    mdAMsg = [
             ""
           , "Phoityne is a ghci debug adapter for Visual Studio Code. "
           , ""
           ]

