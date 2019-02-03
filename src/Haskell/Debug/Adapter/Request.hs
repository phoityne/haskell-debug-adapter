{-# LANGUAGE LambdaCase #-}

module Haskell.Debug.Adapter.Request where

import Control.Monad.IO.Class
import Data.Conduit
import Control.Lens
import Text.Parsec
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Control.Concurrent.MVar
import Control.Monad.State.Lazy
import qualified System.Log.Logger as L
import Control.Monad.Except
import qualified System.IO as S

import qualified GHCi.DAP as DAP
import Haskell.Debug.Adapter.Type
import Haskell.Debug.Adapter.Utility
import Haskell.Debug.Adapter.Constant

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
    pipeline = src .| lbs2req .| sink


---------------------------------------------------------------------------------
-- |
--
src :: ConduitT () B.ByteString AppContext ()
src = do
  liftIO $ L.debugM _LOG_REQUEST $ "src start waiting."
  bs <- lift goApp
  yield bs
  src

  where
    goApp :: AppContext B.ByteString
    goApp = getContentLength >>= getContent
    
    getContent :: Int -> AppContext B.ByteString
    getContent l = getReadHandle >>= flip readBSL l

    getContentLength :: AppContext Int
    getContentLength = go B.empty
        
    go :: B.ByteString -> AppContext Int
    go buf = updateBuf buf >>= findLength

    updateBuf :: B.ByteString -> AppContext B.ByteString
    updateBuf buf = B.append buf <$> getContent 1
    
    findLength :: B.ByteString -> AppContext Int
    findLength buf = case parse parser "find ContentLength parser" (lbs2str buf) of
      Left _  -> go buf
      Right l -> return l

    parser = do
      string _CONTENT_LENGTH
      len <- manyTill digit (string _TWO_CRLF)
      return . read $ len

-- |
--
getReadHandle :: AppContext S.Handle
getReadHandle = view inHandleAppStores <$> get
   >>= isOpenHdl
   >>= isReadableHdl


---------------------------------------------------------------------------------
-- |
--
lbs2req :: ConduitT B.ByteString WrapRequest AppContext ()
lbs2req = do
  liftIO $ L.debugM _LOG_REQUEST $ "lbs2req start waiting."
  await >>= \case
    Nothing  -> do
      throwError $ "[CRITICAL][request][lbs2req] unexpected Nothing."
    Just reqBS -> do
      liftIO $ L.debugM _LOG_REQUEST $ "lbs2req get data. " ++ lbs2str reqBS
      lift (goApp reqBS) >>= \case
        Nothing -> return ()
        Just rq -> yield rq
      lbs2req

  where
    goApp :: B.ByteString -> AppContext (Maybe WrapRequest)
    goApp reqBS = flip catchError errHdl $ do
      req <- decodeRequest reqBS
      reqW <- createWrapRequest reqBS req
      return $ Just reqW

    -- |
    --
    errHdl msg = do
      warnEV _LOG_REQUEST msg
      return Nothing

-- |
--
decodeRequest :: B.ByteString -> AppContext DAP.Request
decodeRequest bs = liftEither $ eitherDecode bs

-- |
--
createWrapRequest :: B.ByteString -> DAP.Request -> AppContext WrapRequest
createWrapRequest bs req 
  | "initialize" == DAP.commandRequest req = WrapRequest . InitializeRequest <$> (liftEither (eitherDecode bs))
  | "launch"     == DAP.commandRequest req = WrapRequest . LaunchRequest <$> (liftEither (eitherDecode bs))
  | "disconnect" == DAP.commandRequest req = WrapRequest . DisconnectRequest <$> (liftEither (eitherDecode bs))
  | "setBreakpoints" == DAP.commandRequest req = WrapRequest . SetBreakpointsRequest <$> (liftEither (eitherDecode bs))
  | "setFunctionBreakpoints" == DAP.commandRequest req = WrapRequest . SetFunctionBreakpointsRequest <$> (liftEither (eitherDecode bs))
  | "setExceptionBreakpoints" == DAP.commandRequest req = WrapRequest . SetExceptionBreakpointsRequest <$> (liftEither (eitherDecode bs))
  | "configurationDone" == DAP.commandRequest req = WrapRequest . ConfigurationDoneRequest <$> (liftEither (eitherDecode bs))
  | "threads" == DAP.commandRequest req = WrapRequest . ThreadsRequest <$> (liftEither (eitherDecode bs))
  | "stackTrace" == DAP.commandRequest req = WrapRequest . StackTraceRequest <$> (liftEither (eitherDecode bs))
  | "scopes" == DAP.commandRequest req = WrapRequest . ScopesRequest <$> (liftEither (eitherDecode bs))
  | "variables" == DAP.commandRequest req = WrapRequest . VariablesRequest <$> (liftEither (eitherDecode bs))
  | otherwise = throwError $ "unsupported request command. " ++ lbs2str bs


---------------------------------------------------------------------------------
-- |
--
sink :: ConduitT WrapRequest Void AppContext ()
sink = do
  liftIO $ L.debugM _LOG_REQUEST $ "sink start waiting."
  await >>= \case
    Nothing  -> do
      throwError $ "[CRITICAL][request][sink] unexpected Nothing."
      return ()
    Just req -> do
      lift $ goApp req
      sink

  where
    -- |
    --
    goApp :: WrapRequest -> AppContext ()
    goApp req = do
      reqs <- view reqStoreAppStores <$> get
      liftIO $ addRequest reqs req


-- |
--
addRequest :: MVar [WrapRequest] -> WrapRequest -> IO ()
addRequest reqsMVar req = do
  reqs <- takeMVar reqsMVar
  putMVar reqsMVar (reqs++[req])
