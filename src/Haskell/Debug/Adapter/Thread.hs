module Haskell.Debug.Adapter.Thread where

import Control.Monad.IO.Class
import Control.Concurrent.Async
import Control.Concurrent
import qualified System.Log.Logger as L
import Data.Maybe
import qualified Data.List as L
import Control.Monad.State.Lazy
import Control.Lens

import Haskell.Debug.Adapter.Type
import Haskell.Debug.Adapter.Utility
import Haskell.Debug.Adapter.Constant
import qualified Haskell.Debug.Adapter.Event as EV


-- |
--
startAll :: [IO ()] -> IO [Async ()]
startAll = mapM async 


-- |
--
stopAll :: [Async ()] -> IO ()
stopAll = mapM_ cancel


-- |
--
start :: AppStores -> [IO ()] -> IO (Async ())
start dat acts = do
  as <- startAll acts
  async $ run dat {_actsAsyncsAppStores = as}


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
  go 
  return ()

  where
    go = isStop >>= \case
      True  -> return ()
      False -> do
        EV.takeEvent >>= \case
          Nothing -> return ()
          Just ev -> EV.runEvent ev
        liftIO $ threadDelay _1_SEC
        go


-- |
--
isStop :: AppContext Bool
isStop = do
  as <- view actsAsyncsAppStores <$> get
  res <- liftIO $ mapM poll as
  -- liftIO $ L.debugM _LOG_THREAD_MGR $ "thread status." ++ show res
  return $ L.all isJust res


