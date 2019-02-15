{-# LANGUAGE LambdaCase #-}

module Haskell.Debug.Adapter.Application where

import Paths_haskell_debug_adapter (version)
import Data.Version (showVersion)
import Control.Monad.IO.Class
import Data.Conduit
import Control.Lens
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Monad.State.Lazy
import Control.Monad.Except
import qualified System.Log.Logger as L
import qualified System.IO as S

import Haskell.Debug.Adapter.Type
import Haskell.Debug.Adapter.Utility
import Haskell.Debug.Adapter.Constant
import Haskell.Debug.Adapter.State.Init()
import Haskell.Debug.Adapter.State.GHCiRun()
import Haskell.Debug.Adapter.State.DebugRun()
import Haskell.Debug.Adapter.State.Shutdown()


-- |
-- 
defaultAppStores :: IO AppStores
defaultAppStores = do
  reqStore <- newMVar []
  resStore <- newMVar []
  evtStore <- newMVar []
  wsStore  <- newEmptyMVar
  logPRStore <- newMVar L.WARNING
  procStore <- newEmptyMVar
  bsStore <- newMVar $ str2bs ""
  verStore <- newEmptyMVar
  return AppStores {
    -- Read Only
      _appNameAppStores     = "haskell-debug-adapter"
    , _appVerAppStores      = showVersion version
    , _inHandleAppStores    = S.stdin
    , _outHandleAppStores   = S.stdout
    , _asyncsAppStores      = []

    -- Read/Write from Application
    , _appStateWAppStores   = WrapAppState InitState
    , _resSeqAppStores      = 0
    , _startupAppStores     = ""
    , _startupFuncAppStores = ""
    , _startupArgsAppStores = ""
    , _stopOnEntryAppStores = False
    , _ghciPmptAppStores    = _GHCI_PROMPT_HDA
    , _mainArgsAppStores    = ""
    , _launchReqSeqAppStores = -1
    -- Read/Write ASync
    , _reqStoreAppStores    = reqStore
    , _resStoreAppStores    = resStore
    , _eventStoreAppStores  = evtStore
    , _workspaceAppStores   = wsStore
    , _logPriorityAppStores = logPRStore
    , _ghciGHCiAppStores    = procStore
    , _ghciStdoutAppStores  = bsStore
    , _ghciVerAppStores     = verStore
    }


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
      criticalEV _LOG_REQUEST msg
      addEvent ShutdownEvent

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
          liftIO $ threadDelay _100_MILLI_SEC
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
    Just req@(WrapRequest ShutdownRequest{}) -> lift $ appMain req
    Just (WrapRequest (DisconnectRequest req)) -> do
      liftIO $ L.infoM _LOG_APP $ "disconnect. end of application thread."
      lift $ sendDisconnectResponse req
    Just req -> do
      lift $ appMain req
      sink


------------------------------------------------------------------------
-- |
--
appMain :: WrapRequest -> AppContext ()
appMain (WrapRequest (TransitRequest (HdaTransitRequest s))) = transit s
appMain reqW = do
  stateW <- view appStateWAppStores <$> get
  
  getStateRequestW stateW reqW >>= actionW >>= \case
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
updateState Init_GHCiRun     = changeState $ WrapAppState GHCiRunState
updateState GHCiRun_DebugRun = changeState $ WrapAppState DebugRunState
updateState GHCiRun_Shutdown = changeState $ WrapAppState ShutdownState
updateState s = throwError $ "not yet implemented state. " ++ show s


-- |
--
changeState :: WrapAppState -> AppContext ()
changeState s = modify $ \d -> d {_appStateWAppStores = s}


