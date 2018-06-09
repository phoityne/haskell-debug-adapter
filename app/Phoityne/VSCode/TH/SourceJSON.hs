{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.SourceJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility

-- |
--   A Source is a descriptor for source code. It is returned from the debug adapter as part of a StackFrame and it is used by clients when specifying breakpoints.
--
data Source =
  Source {
    nameSource             :: Maybe String  -- The short name of the source. Every source returned from the debug adapter has a name. When sending a source to the debug adapter this name is optional.
  , pathSource             :: String        -- The path of the source to be shown in the UI. It is only used to locate and load the content of the source if no sourceReference is specified (or its vaule is 0).
  , sourceReferenceSource  :: Maybe Int     -- If sourceReference > 0 the contents of the source must be retrieved through the SourceRequest (even if a path is specified). A sourceReference is only valid for a session, so it must not be used to persist a source.
  -- , presentationHintSource :: Maybe String  -- An optional hint for how to present the source in the UI. A value of 'deemphasize' can be used to indicate that the source is not available or that it is skipped on stepping.
  , origineSource          :: Maybe String  -- The (optional) origin of this source: possible values 'internal module', 'inlined content from source map', etc.
  -- , sourcesSource          :: [Source]      -- An optional list of sources that are related to this source. These may be the source that generated this source.
  -- , adapterDataSource      :: Maybe String  -- Optional data that a debug adapter might want to loop through the client. The client should leave the data intact and persist it across sessions. The client should not interpret the data.
  -- , checksumsSource        :: []            -- The checksums associated with this file.
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "Source") } ''Source)
