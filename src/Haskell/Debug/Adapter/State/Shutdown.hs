{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Haskell.Debug.Adapter.State.Shutdown where

import Control.Monad.IO.Class
import Control.Monad.Except
import qualified System.Log.Logger as L

import Haskell.Debug.Adapter.Type
import Haskell.Debug.Adapter.Constant


-- | 
--
instance AppStateIF ShutdownState where
  -- |
  --
  entryAction ShutdownState = do
    liftIO $ L.debugM _LOG_APP "ShutdownState entryAction called."
    return ()

  -- |
  --
  exitAction ShutdownState = do
    let msg = "ShutdownState exitAction must not be called."
    liftIO $ L.criticalM _LOG_APP msg
    throwError msg

  -- | 
  --
  getStateRequest ShutdownState _ = do
    let msg = "ShutdownState does not support any request."
    liftIO $ L.criticalM _LOG_APP msg
    throwError msg


