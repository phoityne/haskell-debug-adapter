{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.ColumnDescriptorJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility

-- |
--   A ColumnDescriptor specifies what module attribute to show in a column of the ModulesView, how to format it, and what the column's label should be.
--   It is only used if the underlying UI actually supports this level of customization.
--
data ColumnDescriptor =
  ColumnDescriptor {
    attributeNameColumnDescriptor :: String        -- Name of the attribute rendered in this column. 
  , labelColumnDescriptor         :: String        -- Header UI label of column.
  , formatColumnDescriptor        :: Maybe String  -- Format to use for the rendered values in this column. TBD how the format strings looks like.
  , typeColumnDescriptor          :: Maybe String  -- Datatype of values in this column.  Defaults to 'string' if not specified. 'string' | 'number' | 'boolean' | 'unixTimestampUTC';
  , widthColumnDescriptor         :: Maybe Int     -- Width of this column in characters (hint only).
  } deriving (Show, Read, Eq)


$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "ColumnDescriptor") } ''ColumnDescriptor)
