{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Haskell.Debug.Adapter.State.Init where

import Control.Monad.IO.Class
import Control.Monad.Except
import qualified System.Log.Logger as L

import Haskell.Debug.Adapter.Type
import Haskell.Debug.Adapter.Constant
import Haskell.Debug.Adapter.State.Init.Initialize()
import Haskell.Debug.Adapter.State.Init.Launch()
import Haskell.Debug.Adapter.State.Init.Disconnect()
import qualified Haskell.Debug.Adapter.State.Utility as SU


-- | 
--
instance AppStateIF InitState where
  -- |
  --
  entryAction InitState = do
    let msg = "InitState entryAction must not be called."
    throwError msg

  -- |
  --
  exitAction InitState = do
    liftIO $ L.debugM _LOG_APP "InitState exitAction called."
    return ()

  -- | 
  --
  getStateRequest InitState (WrapRequest (InitializeRequest req))              = return . WrapStateRequest $ Init_Initialize req
  getStateRequest InitState (WrapRequest (LaunchRequest req))                  = return . WrapStateRequest $ Init_Launch req
  getStateRequest InitState (WrapRequest (DisconnectRequest req))              = return . WrapStateRequest $ Init_Disconnect req
  
  getStateRequest InitState (WrapRequest (SetBreakpointsRequest req))          = SU.unsupported $ show req
  getStateRequest InitState (WrapRequest (SetFunctionBreakpointsRequest req))  = SU.unsupported $ show req
  getStateRequest InitState (WrapRequest (SetExceptionBreakpointsRequest req)) = SU.unsupported $ show req
  getStateRequest InitState (WrapRequest (ConfigurationDoneRequest req))       = SU.unsupported $ show req
  getStateRequest InitState (WrapRequest (ThreadsRequest req))                 = SU.unsupported $ show req
  getStateRequest InitState (WrapRequest (StackTraceRequest req))              = SU.unsupported $ show req
  getStateRequest InitState (WrapRequest (ScopesRequest req))                  = SU.unsupported $ show req
  getStateRequest InitState (WrapRequest (VariablesRequest req))               = SU.unsupported $ show req
  getStateRequest InitState (WrapRequest (ContinueRequest req))                = SU.unsupported $ show req
  getStateRequest InitState (WrapRequest (NextRequest req))                    = SU.unsupported $ show req
  getStateRequest InitState (WrapRequest (StepInRequest req))                  = SU.unsupported $ show req

