{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell #-}

module Haskell.Debug.Adapter.Control (
    ArgData(..)
  , run
  ) where

import System.IO
import qualified System.Log.Logger as L
import qualified Control.Exception.Safe as E
import Control.Concurrent.Async
import Data.Default
import Data.Aeson.TH
import Control.Lens
import Data.Data

import Haskell.Debug.Adapter.TH.Utility
import Haskell.Debug.Adapter.Constant
import qualified Haskell.Debug.Adapter.Application as A
import qualified Haskell.Debug.Adapter.Request as RQ
import qualified Haskell.Debug.Adapter.Response as RP
import qualified Haskell.Debug.Adapter.Thread as TD
import qualified Haskell.Debug.Adapter.Watch as W


--------------------------------------------------------------------------------
-- | Command Line Argument Data Type.
--
data ArgData = ArgData {
  } deriving (Data, Typeable, Show, Read, Eq)

makeLenses ''ArgData
$(deriveJSON
  defaultOptions {
      fieldLabelModifier = fieldModifier "ArgData"
    }
  ''ArgData)


-- |
--   default value instance.
--
instance Default ArgData where
  def = ArgData {
      }


-- |
--   start HDA.
--
run :: ArgData -- ^command line arguments type.
    -> Handle  -- ^IN handle. used to get request from the debug adapter client.
    -> Handle  -- ^OUT handle. used to response to the debug adapter client.
    -> IO ()
run _ inHdl outHdl = E.bracket initialize finalize go

  where
    -- |
    --
    initialize = do
      L.debugM _LOG_NAME $ "initialize called."

      hSetBuffering inHdl NoBuffering
      hSetEncoding  inHdl utf8

      hSetBuffering outHdl NoBuffering
      hSetEncoding  outHdl utf8

      A.defaultAppStores inHdl outHdl

    -- |
    --
    finalize _ = do
      L.debugM _LOG_NAME $ "finalize called."
      L.removeAllHandlers

    -- |
    --
    go appData = do

      L.debugM _LOG_NAME $ "start thread manager."

      let ths = [
                  RQ.run appData    -- request handler
                , A.run  appData    -- main app
                , RP.run appData    -- response handler
                , W.run  appData    -- file watch
                ]

      TD.start appData ths >>= wait


