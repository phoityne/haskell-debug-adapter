{-# LANGUAGE OverloadedStrings   #-}

module Haskell.Debug.Adapter.Control where

import System.IO
--import Control.Lens
import qualified System.Log.Logger as L
import qualified Control.Exception.Safe as E
import Control.Concurrent.Async

import Haskell.Debug.Adapter.Constant
import Haskell.Debug.Adapter.Type
--import Haskell.Debug.Adapter.Logger
import qualified Haskell.Debug.Adapter.Application as A
import qualified Haskell.Debug.Adapter.Request as RQ
import qualified Haskell.Debug.Adapter.Response as RP
import qualified Haskell.Debug.Adapter.Thread as TD
import qualified Haskell.Debug.Adapter.Watch as W


-- |
-- 
run :: ArgData -> ConfigData -> IO Int
run _ _ = E.bracket initialize finalize go

  where
    -- |
    -- 
    initialize = do
      hSetBuffering stdin NoBuffering
      hSetEncoding  stdin utf8

      hSetBuffering stdout NoBuffering
      hSetEncoding  stdout utf8

      -- setUpLogger (conf^.logFileConfigData) (conf^.logLevelConfigData)
      L.debugM _LOG_NAME $ "initialize called."

      A.defaultAppStores

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

