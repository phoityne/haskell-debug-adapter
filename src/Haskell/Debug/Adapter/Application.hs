{-# LANGUAGE LambdaCase #-}

module Haskell.Debug.Adapter.Application where

import Control.Monad.IO.Class
import Data.Conduit
import Control.Lens
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Monad.State.Lazy
import Control.Monad.Except
import qualified System.Log.Logger as L

import Haskell.Debug.Adapter.Type
import Haskell.Debug.Adapter.Utility
import Haskell.Debug.Adapter.Constant
import Haskell.Debug.Adapter.State.Utility
import Haskell.Debug.Adapter.State.Init()
import Haskell.Debug.Adapter.State.GHCiRun()
import Haskell.Debug.Adapter.State.DebugRun()
import Haskell.Debug.Adapter.State.Shutdown()
import Haskell.Debug.Adapter.State.Contaminated()


-- |
--
run :: AppStores -> IO ()
run appData = do
  L.debugM _LOG_APP "satrt application app"
  runApp appData app
  L.debugM _LOG_APP "end application app"


-- |
--
app :: AppContext ()
app = flip catchError errHdl $ do
  _ <- runConduit pipeline
  return ()

  where
    pipeline :: ConduitM () Void AppContext ()
    pipeline = src .| sink

    errHdl msg = do
      criticalEV _LOG_APP msg
      addEvent CriticalExitEvent

----------------------------------------------------------------
-- |
--
src :: ConduitT () WrapRequest AppContext ()
src = do
  liftIO $ L.debugM _LOG_APP $ "src start waiting."
  req <- lift goApp
  yield req
  src

  where
    goApp :: AppContext WrapRequest
    goApp = do
      mvar <- view reqStoreAppStores <$> get
      liftIO (takeRequest mvar) >>= \case
        Just res -> return res
        Nothing -> do
          liftIO $ threadDelay _1_MILLI_SEC
          goApp

-- |
--
takeRequest :: MVar [WrapRequest] -> IO (Maybe WrapRequest)
takeRequest reqsMVar = isExists >>= \case
    False -> return Nothing
    True  -> take1

  where
    isExists = readMVar reqsMVar >>= \case
      [] -> return False
      _  -> return True

    take1 = takeMVar reqsMVar >>= \case
      [] -> do
        putMVar reqsMVar []
        return Nothing
      (x:xs) -> do
        putMVar reqsMVar xs
        return $ Just x


------------------------------------------------------------------------
-- |
--
sink :: ConduitT WrapRequest Void AppContext ()
sink = do
  liftIO $ L.debugM _LOG_APP $ "sink start waiting."
  await >>= \case
    Nothing  -> do
      throwError $ "[CRITICAL][response][sink] unexpectHed Nothing."
      return ()
    Just (WrapRequest (DisconnectRequest req)) -> do
      liftIO $ L.debugM _LOG_APP $ show req
      liftIO $ L.infoM _LOG_APP $ "disconnect."
      lift $ sendDisconnectResponse req
      sink
    Just (WrapRequest (PauseRequest req)) -> do
      liftIO $ L.debugM _LOG_APP $ show req
      lift $ sendConsoleEventLF $ "pause request is not supported."
      lift $ sendPauseResponse req
      sink
    Just req -> do
      lift $ appMain req
      sink


------------------------------------------------------------------------
-- |
--
appMain :: WrapRequest -> AppContext ()
appMain (WrapRequest (InternalTransitRequest (HdaInternalTransitRequest s))) = transit s
appMain reqW = do
  stateW <- view appStateWAppStores <$> get
  doActivityW stateW reqW >>= \case
    Nothing -> return ()
    Just st -> transit st

-- |
--
transit :: StateTransit -> AppContext ()
transit st = actExitState
          >> updateState st
          >> actEntryState

  where
    actExitState = do
      stateW <- view appStateWAppStores <$> get
      exitActionW stateW

    actEntryState = do
      stateW <- view appStateWAppStores <$> get
      entryActionW stateW


-- |
--
updateState :: StateTransit -> AppContext ()
updateState Init_GHCiRun          = changeState $ WrapAppState GHCiRunState
updateState Init_Shutdown         = changeState $ WrapAppState ShutdownState
updateState GHCiRun_DebugRun      = changeState $ WrapAppState DebugRunState
updateState GHCiRun_Contaminated  = changeState $ WrapAppState ContaminatedState
updateState GHCiRun_Shutdown      = changeState $ WrapAppState ShutdownState
updateState DebugRun_Contaminated = changeState $ WrapAppState ContaminatedState
updateState DebugRun_Shutdown     = changeState $ WrapAppState ShutdownState
updateState DebugRun_GHCiRun      = changeState $ WrapAppState GHCiRunState
updateState Contaminated_Shutdown = changeState $ WrapAppState ShutdownState


-- |
--
changeState :: WrapAppState -> AppContext ()
changeState s = modify $ \d -> d {_appStateWAppStores = s}


