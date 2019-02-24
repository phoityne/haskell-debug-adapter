{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell #-}

module Haskell.Debug.Adapter.Control where

import System.IO
import Data.Data
import Data.Default
import Control.Lens
import Data.Aeson.TH
import qualified System.Log.Logger as L
import qualified Control.Exception.Safe as E
import Control.Concurrent.Async

import Haskell.Debug.Adapter.Constant
--import Haskell.Debug.Adapter.Type
--import Haskell.Debug.Adapter.Logger
import qualified Haskell.Debug.Adapter.Application as A
import qualified Haskell.Debug.Adapter.Request as RQ
import qualified Haskell.Debug.Adapter.Response as RP
import qualified Haskell.Debug.Adapter.Thread as TD
import qualified Haskell.Debug.Adapter.Watch as W
import Haskell.Debug.Adapter.TH.Utility


--------------------------------------------------------------------------------
-- | Command Line Argument Data Type.
--
data ArgData = ArgData {
    _hackageVersionArgData :: String     -- ^hackage library version.
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
        _hackageVersionArgData = ""
      }


-- |
--   start running HDA.
--
run :: ArgData   -- ^command line arguments type.
    -> Handle    -- ^IN handle. used to get request from debug adapter client.
    -> Handle    -- ^OUT handle. used to response to debug adapter client.
    -> IO Int    -- ^Exit code.
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
                  RQ.run appData
                , A.run appData
                , RP.run appData
                , W.run appData
                ]

      TD.start appData ths >>= wait

      return 0

