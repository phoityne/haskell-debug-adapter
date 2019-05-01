{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Haskell.Debug.Adapter.State.Shutdown where

import Control.Monad.IO.Class
import Control.Monad.Except
import qualified System.Log.Logger as L

import Haskell.Debug.Adapter.Type
import Haskell.Debug.Adapter.Utility
import Haskell.Debug.Adapter.Constant


-- |
--
instance AppStateIF ShutdownStateData where
  -- |
  --
  entryAction ShutdownState = do
    liftIO $ L.debugM _LOG_APP "ShutdownState entryAction called."

    addEvent CriticalExitEvent

    return ()


  -- |
  --
  exitAction ShutdownState = do
    let msg = "ShutdownState exitAction must not be called."
    throwError msg


  -- |
  --
  doActivity ShutdownState _ = do
    let msg = "ShutdownState does not support any request."
    infoEV _LOG_APP msg
    return Nothing

