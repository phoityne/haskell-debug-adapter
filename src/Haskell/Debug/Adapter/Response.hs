{-# LANGUAGE LambdaCase #-}

module Haskell.Debug.Adapter.Response where

import Control.Monad.IO.Class
import Data.Conduit
import Control.Lens
import Data.Aeson
import Control.Concurrent (threadDelay)
import qualified Data.ByteString.Lazy as B
import Control.Concurrent.MVar
import Control.Monad.State.Lazy
import qualified System.Log.Logger as L
import Control.Monad.Except
import qualified System.IO as S
import qualified Data.List as L

import Haskell.Debug.Adapter.Type
import Haskell.Debug.Adapter.Utility
import Haskell.Debug.Adapter.Constant


-- |
--
run :: AppStores -> IO ()
run appData = do
  L.debugM _LOG_RESPONSE "start response app"
  _ <- runApp appData app
  L.debugM _LOG_RESPONSE "end response app"


-- |
--
app :: AppContext ()
app = flip catchError errHdl $ do
  _ <- runConduit pipeline
  return ()

  where
    pipeline :: ConduitM () Void AppContext ()
    pipeline = src .| res2lbs .| sink

    errHdl msg = do
      criticalEV _LOG_REQUEST msg
      addEvent CriticalExitEvent

---------------------------------------------------------------------------------
-- |
--
src :: ConduitT () Response AppContext ()
src = do
  liftIO $ L.debugM _LOG_RESPONSE $ "src start waiting."
  res <- lift goApp
  yield res
  src


  where
    goApp :: AppContext Response
    goApp = get >>= liftIO . getResponse

-- |
--
getResponse :: AppStores -> IO Response
getResponse appDat = takeResponse appDat >>= \case
  Just res -> return res
  Nothing -> do
    threadDelay _1_MILLI_SEC
    getResponse appDat


-- |
--
takeResponse :: AppStores -> IO (Maybe Response)
takeResponse appData = do
  let ressMVar = appData^.resStoreAppStores
  isExists ressMVar >>= \case
    False -> return Nothing
    True  -> take1 ressMVar

  where
    isExists ressMVar = readMVar ressMVar >>= \case
      [] -> return False
      _  -> return True

    take1 ressMVar = takeMVar ressMVar >>= \case
      [] -> do
        putMVar ressMVar []
        return Nothing
      (x:xs) -> do
        putMVar ressMVar xs
        return $ Just x


---------------------------------------------------------------------------------
-- |
--
res2lbs :: ConduitT Response B.ByteString AppContext ()
res2lbs = do
  liftIO $ L.debugM _LOG_RESPONSE $ "res2lbs start waiting."
  await >>= \case
    Nothing -> do
      throwError $ "[CRITICAL][response][res2lbs] unexpectHed Nothing."
      return ()
    Just res -> do
      liftIO $ L.debugM _LOG_RESPONSE $ "res2lbs get data. " ++ show res
      bs <- lift $ goApp res
      yield bs
      res2lbs
  
  where
    goApp :: Response -> AppContext B.ByteString
    goApp = return . encode


---------------------------------------------------------------------------------
-- |
--
sink :: ConduitT B.ByteString Void AppContext ()
sink = do
  liftIO $ L.debugM _LOG_RESPONSE $ "sink start start."
  await >>= \case
    Nothing  -> do
      throwError $ "[CRITICAL][response][sink] unexpectHed Nothing."
      return ()
    Just bs -> do
      liftIO $ L.debugM _LOG_RESPONSE $ "sink get data. " ++ lbs2str bs
      lift $ goApp bs
      cont $ lbs2str bs

  where
    goApp bs = do
      wHdl <- view outHandleAppStores <$> get
      liftIO $ sendResponse wHdl bs

    cont str
      | L.isInfixOf _KEY_DISCONNECT_RESPONCE str = do
        liftIO $ L.infoM _LOG_RESPONSE $ "disconnect. end of response thread."
      | otherwise = sink

-- |
--
_KEY_DISCONNECT_RESPONCE :: String
_KEY_DISCONNECT_RESPONCE = "\"command\":\"disconnect\""

-- |
--
sendResponse :: S.Handle -> B.ByteString -> IO ()
sendResponse hdl str = do
  B.hPut hdl $ str2lbs $ _CONTENT_LENGTH ++ (show (B.length str))
  B.hPut hdl $ str2lbs _TWO_CRLF
  B.hPut hdl str
  S.hFlush hdl

