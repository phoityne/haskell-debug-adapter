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
    , _actsAsyncsAppStores  = []

    -- Read/Write from Application
    , _appStateWAppStores   = WrapAppState InitState
    , _resSeqAppStores      = 0
    , _startupAppStores     = ""
    , _startupFuncAppStores = ""
    , _startupArgsAppStores = ""
    , _stopOnEntryAppStores = False
    , _ghciPmptAppStores    = _GHCI_PROMPT_HDA
    , _mainArgsAppStores    = ""

    -- Read/Write ASync
    , _reqStoreAppStores    = reqStore
    , _resStoreAppStores    = resStore
    , _evtStoreAppStores    = evtStore
    , _workspaceAppStores   = wsStore
    , _logPriorityAppStores = logPRStore
    , _ghciGHCiAppStores    = procStore
    , _ghciStdoutAppStores  = bsStore
    , _ghciVerAppStores     = verStore
    }


-- |
--
run :: AppStores -> IO ()
run appData = runApp appData app >>= \case
    Left err -> do
      L.errorM _LOG_NAME err
      return ()
    Right (res, _) -> do
      L.infoM _LOG_NAME $ show res
      return ()


-- |
--
app :: AppContext ()
app = do
  _ <- runConduit pipeline
  return ()

  where
    pipeline :: ConduitM () Void AppContext ()
    pipeline = src .| sink


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
    goApp = get >>= liftIO . getRequest


-- |
--
getRequest :: AppStores -> IO WrapRequest
getRequest appDat = takeRequest appDat >>= \case
  Just res -> return res
  Nothing -> do
    threadDelay (100 * 1000)
    getRequest appDat


-- |
--
takeRequest :: AppStores -> IO (Maybe WrapRequest)
takeRequest appData = do
  let reqsMVar = appData^.reqStoreAppStores
  isExists reqsMVar >>= \case
    False -> return Nothing
    True  -> take1 reqsMVar

  where
    isExists reqsMVar = readMVar reqsMVar >>= \case
      [] -> return False
      _  -> return True

    take1 reqsMVar = takeMVar reqsMVar >>= \case
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
      throwError $ "[CRITICAL][response][sink] unexpected Nothing."
      return ()
    Just req -> do
      lift $ appMain req
      sink


------------------------------------------------------------------------
-- |
--
appMain :: WrapRequest -> AppContext ()
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


