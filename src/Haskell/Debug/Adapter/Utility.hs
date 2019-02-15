module Haskell.Debug.Adapter.Utility where

import System.IO
import Control.Lens
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Conduit.Binary as C
import qualified Data.Conduit as C
import qualified Data.Conduit.List as C
import Control.Monad.Except
import Control.Monad.State.Lazy
import qualified System.IO as S
import Control.Concurrent.MVar
import qualified System.Log.Logger as L
import qualified Data.List as L
import qualified Control.Exception.Safe as E


import qualified GHCi.DAP as DAP
import Haskell.Debug.Adapter.Type
import Haskell.Debug.Adapter.Constant

-- |
--
str2bs :: String -> BS.ByteString
str2bs = TE.encodeUtf8 . T.pack

-- |
--
bs2str :: BS.ByteString -> String
bs2str = T.unpack. TE.decodeUtf8

-- |
--
str2lbs :: String -> BSL.ByteString
str2lbs = TLE.encodeUtf8 . TL.pack

-- |
--
lbs2str :: BSL.ByteString -> String
lbs2str = TL.unpack. TLE.decodeUtf8


-- |
--  
-- 
loadFile :: FilePath -> IO BS.ByteString
loadFile path = do
  bs <- C.runConduitRes
      $ C.sourceFile path
      C..| C.consume
  return $ BS.concat bs


-- |
--  
-- 
saveFile :: FilePath -> BS.ByteString -> IO ()
saveFile path cont = saveFileBSL path $ BSL.fromStrict cont


-- |
--  
-- 
saveFileBSL :: FilePath -> BSL.ByteString -> IO ()
saveFileBSL path cont = C.runConduitRes
  $ C.sourceLbs cont
  C..| C.sinkFile path


-- |
--  
-- 
add2File :: FilePath -> BS.ByteString -> IO ()
add2File path cont = add2FileBSL path $ BSL.fromStrict cont

-- |
--  
-- 
add2FileBSL :: FilePath -> BSL.ByteString -> IO ()
add2FileBSL path cont = C.runConduitRes
  $ C.sourceLbs cont
  C..| C.sinkIOHandle hdl

  where hdl = openFile path AppendMode

           
-- |
--  utility
-- 
showEE :: (Show e) => Either e a -> Either ErrMsg a
showEE (Right v) = Right v
showEE (Left  e) = Left $ show e


-- |
-- 
runApp :: AppStores -> AppContext a -> IO (Either ErrMsg (a, AppStores))
runApp dat app = runExceptT $ runStateT app dat


-- |
--
addRequest :: WrapRequest -> AppContext ()
addRequest req = do
  reqsMVar <- view reqStoreAppStores <$> get
  reqs <- liftIO $ takeMVar reqsMVar
  liftIO $ putMVar reqsMVar (reqs++[req])


-- |
--  High priority
--
addRequestHP :: WrapRequest -> AppContext ()
addRequestHP req = do
  reqsMVar <- view reqStoreAppStores <$> get
  reqs <- liftIO $ takeMVar reqsMVar
  liftIO $ putMVar reqsMVar (req:reqs)


-- |
--
addResponse :: Response -> AppContext ()
addResponse res = do
  appData <- get
  let mvar = appData^.resStoreAppStores
  ress <- liftIO $ takeMVar mvar
  liftIO $ putMVar mvar (ress++[res])


-- |
--
getIncreasedResponseSequence :: AppContext Int
getIncreasedResponseSequence = do
  appData <- get
  let cnt = appData^.resSeqAppStores
      seq = 1 + cnt
  put appData {_resSeqAppStores = seq}
  return seq

-- |
--
sendConsoleEvent :: String -> AppContext ()
sendConsoleEvent = sendOutputEventWithType "console"

-- |
--
sendConsoleEventLF :: String -> AppContext ()
sendConsoleEventLF x = sendConsoleEvent (x ++ _LF_STR)


-- |
--
sendStdoutEvent :: String -> AppContext ()
sendStdoutEvent = sendOutputEventWithType "stdout"


-- |
--
sendStdoutEventLF :: String -> AppContext ()
sendStdoutEventLF x = sendStdoutEvent (x ++ _LF_STR)


-- |
--
sendErrorEvent :: String -> AppContext ()
sendErrorEvent = sendOutputEventWithType "stderr"

-- |
--
sendErrorEventLF :: String -> AppContext ()
sendErrorEventLF x = sendErrorEvent (x ++ _LF_STR)


-- |
--
sendOutputEventWithType :: String -> String -> AppContext ()
sendOutputEventWithType evType msg = do
  resSeq <- getIncreasedResponseSequence
  let body = DAP.OutputEventBody evType msg Nothing
      outEvt = DAP.defaultOutputEvent {
                DAP.seqOutputEvent = resSeq
              , DAP.bodyOutputEvent = body
              }
              
  addResponse $ OutputEvent outEvt

-- |
--
debugEV :: String -> String -> AppContext ()
debugEV name msg = do
  liftIO $ L.debugM name msg
  logEV L.DEBUG name msg

-- |
--
infoEV :: String -> String -> AppContext ()
infoEV name msg = do
  liftIO $ L.infoM name msg
  logEV L.INFO name msg

-- |
--
warnEV :: String -> String -> AppContext ()
warnEV name msg = do
  liftIO $ L.warningM name msg
  logEV L.WARNING name msg

-- |
--
errorEV :: String -> String -> AppContext ()
errorEV name msg = do
  liftIO $ L.errorM name msg
  logEV L.ERROR name msg

-- |
--
criticalEV :: String -> String -> AppContext ()
criticalEV name msg = do
  liftIO $ L.criticalM name msg
  logEV L.CRITICAL name msg

-- |
--
logEV :: L.Priority -> String -> String -> AppContext ()
logEV pr name msg = do
  mvar <- view logPriorityAppStores <$> get
  logPR <- liftIO $ readMVar mvar
  let msg' = if L.isSuffixOf _LF_STR msg then msg else msg ++ _LF_STR
   
  when (pr >= logPR) $ do
    sendStdoutEvent $ "[" ++ show pr ++ "][" ++ name ++ "] " ++ msg'


-- |
--
sendDisconnectResponse :: DAP.DisconnectRequest -> AppContext ()
sendDisconnectResponse req = do
  resSeq <- getIncreasedResponseSequence

  let res = DAP.defaultDisconnectResponse {
            DAP.seqDisconnectResponse         = resSeq
          , DAP.request_seqDisconnectResponse = DAP.seqDisconnectRequest req
          , DAP.successDisconnectResponse     = True
          }

  addResponse $ DisconnectResponse res

-- |
--
--   phoityne -> haskell-dap
--   encoding RequestArgument to [Word8] because of using ghci command line interface.
--
showDAP :: Show a => a -> String
showDAP = show . BS.unpack . TE.encodeUtf8 . T.pack . show


-- |
--
sendTerminateEvent :: AppContext ()
sendTerminateEvent = do
  resSeq <- getIncreasedResponseSequence
  let evt = DAP.defaultTerminatedEvent {
            DAP.seqTerminatedEvent = resSeq
          }

  addResponse $ TerminatedEvent evt


-- |
--
handleStoppeEventBody :: DAP.StoppedEventBody -> AppContext ()
handleStoppeEventBody body 
  | "complete" == DAP.reasonStoppedEventBody body = do
    debugEV _LOG_NAME $ "debugging completed. " ++ show body
    -- must terminate. can not back to GHCiState.
    sendTerminateEvent
  | otherwise = do
    resSeq <- getIncreasedResponseSequence
    let res = DAP.defaultStoppedEvent {
              DAP.seqStoppedEvent = resSeq
            , DAP.bodyStoppedEvent = body
            }

    addResponse $ StoppedEvent res


-- |
--
readLine :: S.Handle -> AppContext BS.ByteString
readLine hdl =   isOpenHdl hdl
             >>= isReadableHdl
             >>= isNotEofHdl
             >>= go

  where
    go hdl = liftIO (goIO hdl) >>= liftEither
    
    goIO hdl = flip E.catchAny errHdl $ do
      Right <$> BS.hGetLine hdl

    errHdl = return . Left . show

-- |
--
readChar :: S.Handle -> AppContext BS.ByteString
readChar hdl = readChars hdl 1


-- |
--
readChars :: S.Handle -> Int -> AppContext BS.ByteString
readChars hdl c = isOpenHdl hdl
              >>= isReadableHdl
              >>= isNotEofHdl
              >>= go
              >>= isNotEmpty

  where
    go hdl = liftIO (goIO hdl) >>= liftEither
    
    goIO hdl = flip E.catchAny errHdl $ do
      Right <$> BS.hGet hdl c

    errHdl = return . Left . show

    

-- |
--
readCharL :: S.Handle -> AppContext BSL.ByteString
readCharL hdl = readCharsL hdl 1


-- |
--
readCharsL :: S.Handle -> Int -> AppContext BSL.ByteString
readCharsL hdl c = isOpenHdl hdl
               >>= isReadableHdl
               >>= isNotEofHdl
               >>= go
               >>= isNotEmptyL

  where
    go hdl = liftIO (goIO hdl) >>= liftEither

    goIO hdl = flip E.catchAny errHdl $ do
      Right <$> BSL.hGet hdl c

    errHdl = return . Left . show


-- |
--
isOpenHdl :: S.Handle -> AppContext S.Handle
isOpenHdl rHdl = liftIO (S.hIsOpen rHdl) >>= \case
  True  -> return rHdl
  False -> throwError "invalid HANDLE. not opened."


-- |
--
isReadableHdl :: S.Handle -> AppContext S.Handle
isReadableHdl rHdl = liftIO (S.hIsReadable rHdl) >>= \case
  True  -> return rHdl
  False -> throwError "invalid HANDLE. not readable."


-- |
--
isNotEofHdl :: S.Handle -> AppContext S.Handle
isNotEofHdl rHdl = liftIO (S.hIsEOF rHdl) >>= \case
  False -> return rHdl
  True  -> throwError "invalid HANDLE. eof."


-- |
--
isNotEmpty :: BS.ByteString -> AppContext BS.ByteString
isNotEmpty b
  | b == BS.empty = throwError "empty input."
  | otherwise = return b


-- |
--
isNotEmptyL :: BSL.ByteString -> AppContext BSL.ByteString
isNotEmptyL b
  | b == BSL.empty = throwError "empty input."
  | otherwise = return b


-- |
--
addEvent :: Event -> AppContext ()
addEvent evt = do
  mvar <- view eventStoreAppStores <$> get
  evts <- liftIO $ takeMVar mvar
  liftIO $ putMVar mvar (evts++[evt])

