{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}

module Haskell.Debug.Adapter.Watch where

import Control.Monad.IO.Class
import qualified System.FSNotify as S
import Control.Lens
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Monad.State.Lazy
import qualified System.Log.Logger as L
import Control.Monad.Except
import qualified Data.List as L

import Haskell.Debug.Adapter.Type
import Haskell.Debug.Adapter.Utility
import Haskell.Debug.Adapter.Constant

import System.FilePath

#if __GLASGOW_HASKELL__ >= 906
import Control.Monad
#endif
-- |
--
run :: AppStores -> IO ()
run appData = do
  L.debugM _LOG_WATCH "start watch app"
  _ <- runApp appData app
  L.debugM _LOG_WATCH "end watch app"


-- |
--
app :: AppContext ()
app = flip catchError errHdl $ do
  liftIO $ L.infoM _LOG_WATCH "wait getting workspace path."
  ws <- getWS
  liftIO $ L.infoM _LOG_WATCH $ "start watching " ++ ws

  reqStore <- view reqStoreAppStores <$> get
  let conf = S.defaultConfig
  liftIO $ S.withManagerConf conf $ goIO ws reqStore
  
  where
    -- |
    --
    errHdl msg = do
      criticalEV _LOG_REQUEST msg
      addEvent CriticalExitEvent

    -- |
    --
    goIO ws reqStore mgr = do
      S.watchTree mgr ws hsFilter (action reqStore)
      forever $ threadDelay _1_SEC

    -- |
    --
    hsFilter ev = (L.isSuffixOf _HS_FILE_EXT (S.eventPath ev))
               && (not (L.isInfixOf (pathSeparator:".") (S.eventPath ev)))

    -- |
    --
    action mvar ev@(S.Added{}) = sendRequest mvar ev
    action mvar ev@(S.Modified{}) = sendRequest mvar ev
    action _ _ = return ()

    -- |
    --
    sendRequest mvar ev = do
      L.debugM _LOG_WATCH $ "detect. " ++ show ev
      let req = WrapRequest $ InternalLoadRequest 
              $ HdaInternalLoadRequest $ S.eventPath ev
      reqs <- takeMVar mvar
      putMVar mvar (req : reqs)
  

    -- |
    -- 
    getWS :: AppContext FilePath
    getWS = do
      wsMVar <- view workspaceAppStores <$> get
      ws <- liftIO $ readMVar wsMVar
      if not (null ws) then return ws
        else do
          liftIO $ threadDelay _1_SEC
          getWS

