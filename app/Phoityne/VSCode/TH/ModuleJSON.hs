{-# LANGUAGE TemplateHaskell #-}


module Phoityne.VSCode.TH.ModuleJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility

-- |
--   A Module object represents a row in the modules view.
--   Two attributes are mandatory: an id identifies a module in the modules view and is used in a ModuleEvent for identifying a module for adding, updating or deleting.
--   The name is used to minimally render the module in the UI.
--
--   Additional attributes can be added to the module. They will show up in the module View if they have a corresponding ColumnDescriptor.
--   To avoid an unnecessary proliferation of additional attributes with similar semantics but different names
--   we recommend to re-use attributes from the 'recommended' list below first, and only introduce new attributes if nothing appropriate could be found.
--
--
data Module =
  Module {
    idModule             :: String         -- Unique identifier for the module.
  , nameModle            :: String         -- A name of the module.
  -- optional but recommended attributes.
  -- always try to use these first before introducing additional attributes.
  -- Logical full path to the module. The exact definition is implementation defined, but usually this would be a full path to the on-disk file for the module.
  , pathModle            :: String
  , versionModule        :: Maybe String    -- Version of Module.
  , isUserCodeModule     :: Bool            -- True if the module is considered 'user code' by a debugger that supports 'Just My Code'.
  , symbolStatusModule   :: Maybe String    -- User understandable description of if symbols were found for the module (ex: 'Symbols Loaded', 'Symbols not found', etc. 
  , symbolFilePathModule :: Maybe FilePath  -- Logical full path to the symbol file. The exact definition is implementation defined.
  , dateTimeStampModule  :: Maybe String    -- Module created or modified.
  , addressRangeModule   :: Maybe String    -- Address range covered by this module.
  } deriving (Show, Read, Eq)


$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "Module") } ''Module)
