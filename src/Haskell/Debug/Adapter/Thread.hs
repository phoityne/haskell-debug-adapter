module Haskell.Debug.Adapter.Thread where

import Control.Monad.IO.Class
import Control.Concurrent.Async
import Control.Concurrent
import qualified System.Log.Logger as L
import Data.Maybe
import qualified Data.List as L
import Control.Monad.State.Lazy
import Control.Lens
import Control.Monad.Except

import Haskell.Debug.Adapter.Type
import Haskell.Debug.Adapter.Utility
import Haskell.Debug.Adapter.Constant


-- |
--
start :: AppStores -> [IO ()] -> IO (Async ())
start dat acts = do
  as <- mapM async acts
  async $ run dat {_asyncsAppStores = as}



-- |
--
run :: AppStores -> IO ()
run appData = do
  L.debugM _LOG_APP "satrt thread manager"
  runApp appData app
  L.debugM _LOG_APP "stop thread manager"


-- |
--
app :: AppContext ()
app = catchError go errHdl
  where
    errHdl msg = do
      criticalEV _LOG_THREAD_MGR msg
      criticalEV _LOG_THREAD_MGR "stopping all threads."
      as <- view asyncsAppStores <$> get
      liftIO $ mapM_ cancel as
      
    go = isStop >>= \case
      True  -> return ()
      False -> do
        takeEvent >>= \case
          Nothing -> return ()
          Just ev -> runEvent ev
        liftIO $ threadDelay _1_SEC
        go


-- |
--
isStop :: AppContext Bool
isStop = do
  as <- view asyncsAppStores <$> get
  res <- liftIO $ mapM poll as
  -- liftIO $ L.debugM _LOG_THREAD_MGR $ "thread status." ++ show res
  return $ L.all isJust res


-- |
--
takeEvent :: AppContext (Maybe Event)
takeEvent = do
  appDat <- get
  let mvar = appDat^.eventStoreAppStores
  liftIO $ goIO mvar


  where
    goIO mvar = isExists mvar >>= \case
      False -> return Nothing
      True  -> take1 mvar

    isExists mvar = readMVar mvar >>= \case
      [] -> return False
      _  -> return True

    take1 mvar = takeMVar mvar >>= \case
      [] -> do
        putMVar mvar []
        return Nothing
      (x:xs) -> do
        putMVar mvar xs
        return $ Just x


-- |
--
runEvent :: Event -> AppContext ()
runEvent ShutdownEvent = do
  liftIO $ L.infoM _LOG_NAME "Shutdown started."
  as <- view asyncsAppStores <$> get

  addRequestHP $ WrapRequest 
               $ ShutdownRequest
               $ HdaShutdownRequest "shutdown event."

  liftIO $ threadDelay _1_SEC
  liftIO $ threadDelay _1_SEC

  b <- isStop
  when (False == b) $ do
    liftIO $ L.infoM _LOG_NAME "Shutdown force."
    liftIO $ mapM_ cancel as
