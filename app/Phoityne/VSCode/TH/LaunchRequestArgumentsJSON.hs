{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.LaunchRequestArgumentsJSON where

import Data.Aeson.TH
import qualified Data.Map as M
import Phoityne.VSCode.Utility

-- |
--   Arguments for "launch" request.
--
data LaunchRequestArguments =
  LaunchRequestArguments {
    noDebugLaunchRequestArguments      :: Maybe Bool -- If noDebug is true the launch request should launch the program without enabling debugging.
  , nameLaunchRequestArguments         :: String     -- Phoityne specific argument. required.
  , typeLaunchRequestArguments         :: String     -- Phoityne specific argument. required.
  , requestLaunchRequestArguments      :: String     -- Phoityne specific argument. required. must be "request"
  , startupLaunchRequestArguments      :: String     -- Phoityne specific argument. required.
  , workspaceLaunchRequestArguments    :: String     -- Phoityne specific argument. required.
  , logFileLaunchRequestArguments      :: String     -- Phoityne specific argument. required.
  , logLevelLaunchRequestArguments     :: String     -- Phoityne specific argument. required.
  , ghciPromptLaunchRequestArguments   :: String     -- Phoityne specific argument. required.
  , ghciCmdLaunchRequestArguments      :: String     -- Phoityne specific argument. required.
  , stopOnEntryLaunchRequestArguments  :: Bool       -- Phoityne specific argument. required.
  , mainArgsLaunchRequestArguments     :: Maybe String         -- Phoityne specific argument. required.
  , ghciEnvLaunchRequestArguments      :: M.Map String String  -- Phoityne specific argument. required.
  , ghciInitialPromptLaunchRequestArguments  :: Maybe String     -- Phoityne specific argument. required.
  , startupFuncLaunchRequestArguments  :: Maybe String
  , startupArgsLaunchRequestArguments  :: Maybe String
  } deriving (Show, Read, Eq)


$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "LaunchRequestArguments") } ''LaunchRequestArguments)

