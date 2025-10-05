{-# LANGUAGE CPP #-}

module Haskell.Debug.Adapter.Utility where

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
import qualified System.Process as S
import qualified System.Exit as S
import Control.Concurrent.MVar
import qualified System.Log.Logger as L
import qualified Data.List as L
import qualified Control.Exception.Safe as E

#if __GLASGOW_HASKELL__ >= 906
import Control.Monad
#endif

import qualified Haskell.DAP as DAP
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

  where hdl = S.openFile path S.AppendMode


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
sendPauseResponse :: DAP.PauseRequest -> AppContext ()
sendPauseResponse req = do
  resSeq <- getIncreasedResponseSequence

  let res = DAP.defaultPauseResponse {
            DAP.seqPauseResponse         = resSeq
          , DAP.request_seqPauseResponse = DAP.seqPauseRequest req
          , DAP.successPauseResponse     = False
          , DAP.messagePauseResponse     = "pause request is not supported."
          }

  addResponse $ PauseResponse res


-- |
--
--   phoityne -> haskell-dap
--   encoding RequestArgument to [Word8] because of using ghci command line interface.
--
showDAP :: Show a => a -> String
showDAP = show . BS.unpack . TE.encodeUtf8 . T.pack . show


-- |
--
sendTerminatedEvent :: AppContext ()
sendTerminatedEvent = do
  resSeq <- getIncreasedResponseSequence
  let evt = DAP.defaultTerminatedEvent {
            DAP.seqTerminatedEvent = resSeq
          }

  addResponse $ TerminatedEvent evt


-- |
--
sendRestartEvent :: AppContext ()
sendRestartEvent = do
  resSeq <- getIncreasedResponseSequence
  let evt = DAP.defaultTerminatedEvent {
            DAP.seqTerminatedEvent = resSeq
          , DAP.bodyTerminatedEvent = DAP.defaultTerminatedEventBody {
              DAP.restartTerminatedEventBody = True
            }
          }

  addResponse $ TerminatedEvent evt



-- |
--
sendExitedEvent :: AppContext ()
sendExitedEvent = do
  code <- getExitCode
  resSeq <- getIncreasedResponseSequence
  let evt = DAP.defaultExitedEvent {
            DAP.seqExitedEvent = resSeq
          , DAP.bodyExitedEvent = DAP.defaultExitedEventBody {
                DAP.exitCodeExitedEventBody = code
              }
          }

  addResponse $ ExitedEvent evt

  where
    getExitCode = getGHCiExitCode >>= \case
      Just c -> return c
      Nothing -> do
        liftIO $ L.infoM _LOG_NAME "force kill ghci."
        force

    force = killGHCi >> getGHCiExitCode >>= \case
      Just c -> return c
      Nothing -> do
        liftIO $ L.infoM _LOG_NAME "force kill ghci failed."
        return 1  -- kill ghci failed. error exit anyway.

-- |
--
getGHCiExitCode :: AppContext (Maybe Int)
getGHCiExitCode = do
  procMVar <- view ghciProcAppStores <$> get
  proc <- liftIO $ readMVar procMVar
  liftIO (S.getProcessExitCode (proc^.procGHCiProc)) >>= \case
    Just S.ExitSuccess -> return $ Just 0
    Just (S.ExitFailure c) -> return $ Just c
    Nothing -> return Nothing

-- |
--   On Windows, terminateProcess blocks for exiting.
--
killGHCi :: AppContext ()
killGHCi = do
  return ()
  {-
  procMVar <- view ghciProcAppStores <$> get
  proc <- liftIO $ readMVar procMVar
  liftIO $ S.terminateProcess (proc^.procGHCiProc)
  -}

-- |
--
handleStoppedEventBody :: DAP.StoppedEventBody -> AppContext ()
handleStoppedEventBody body
  | "complete" == DAP.reasonStoppedEventBody body = do
      sendConsoleEventLF "debugging completed. "
      isReRun <- view debugReRunableAppStores <$> get
      handleReRun isReRun
  | otherwise = sendStoppedEvent

  where
    handleReRun True = do
      -- ghci and vscode can not rerun debugging without restart.
      -- sendContinuedEvent
      -- sendPauseEvent
      addRequestHP $ WrapRequest
                   $ InternalTransitRequest
                   $ HdaInternalTransitRequest DebugRun_Contaminated

    handleReRun False = do
      addRequestHP $ WrapRequest
                   $ InternalTerminateRequest
                   $ HdaInternalTerminateRequest ""

    sendStoppedEvent = do
      resSeq <- getIncreasedResponseSequence
      let res = DAP.defaultStoppedEvent {
                DAP.seqStoppedEvent = resSeq
              , DAP.bodyStoppedEvent = body
              }

      addResponse $ StoppedEvent res

{-
    sendContinuedEvent = do
      resSeq <- getIncreasedResponseSequence
      let res = DAP.defaultContinuedEvent {
                DAP.seqContinuedEvent = resSeq
              }

      addResponse $ ContinuedEvent res

    sendPauseEvent = do
      resSeq <- getIncreasedResponseSequence
      let res = DAP.defaultStoppedEvent {
                DAP.seqStoppedEvent = resSeq
              , DAP.bodyStoppedEvent = DAP.defaultStoppedEventBody {
                  DAP.reasonStoppedEventBody = "pause"
                }
              }

      addResponse $ StoppedEvent res
-}

-- |
--
readLine :: S.Handle -> AppContext String
readLine hdl =   isOpenHdl hdl
             >>= isReadableHdl
             >>= isNotEofHdl
             >>= go

  where
    go hdl = liftIOE $ S.hGetLine hdl


-- |
--
readChar :: S.Handle -> AppContext String
readChar hdl = isOpenHdl hdl
           >>= isReadableHdl
           >>= isNotEofHdl
           >>= go
           >>= toString
           >>= isNotEmpty

  where
    go hdl = liftIOE $ S.hGetChar hdl

    toString c = return [c]


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
    go hdl = liftIOE $ BSL.hGet hdl c


-- |
--
isOpenHdl :: S.Handle -> AppContext S.Handle
isOpenHdl rHdl = liftIOE (S.hIsOpen rHdl) >>= \case
  True  -> return rHdl
  False -> throwError "invalid HANDLE. not opened."


-- |
--
isReadableHdl :: S.Handle -> AppContext S.Handle
isReadableHdl rHdl = liftIOE (S.hIsReadable rHdl) >>= \case
  True  -> return rHdl
  False -> throwError "invalid HANDLE. not readable."


-- |
--
isNotEofHdl :: S.Handle -> AppContext S.Handle
isNotEofHdl rHdl = liftIOE (S.hIsEOF rHdl) >>= \case
  False -> return rHdl
  True  -> throwError "invalid HANDLE. eof."


-- |
--
isNotEmpty :: String -> AppContext String
isNotEmpty b
  | null b = throwError "empty input."
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


-- |
--
liftIOE :: IO a -> AppContext a
liftIOE f = liftIO (go f) >>= liftEither
  where
    go :: IO b -> IO (Either String b)
    go f = E.catchAny (Right <$> f) errHdl

    errHdl :: E.SomeException -> IO (Either String a)
    errHdl = return . Left . show


-- |
--
rstrip :: String -> String
rstrip = T.unpack . T.stripEnd . T.pack


-- |
--
strip :: String -> String
strip = T.unpack . T.strip . T.pack

-- |
--
replace :: String -> String -> String -> String
replace a b c = T.unpack $ T.replace (T.pack a) (T.pack b) (T.pack c)

-- |
--
split :: String -> String -> [String]
split a b = map T.unpack $ T.splitOn (T.pack a) (T.pack b)

-- |
--
join :: String -> [String] -> String
join a b = T.unpack $ T.intercalate (T.pack a) $ map T.pack b

-- |
--
startswith :: String -> String -> Bool
startswith a b = T.isPrefixOf (T.pack a) (T.pack b)


-- |
--
stdioLogging :: BSL.ByteString -> AppContext ()
stdioLogging bs = do
  logFile <- view stdioLogFileAppStores <$> get
  go logFile

  where
    go :: Maybe FilePath -> AppContext ()
    go Nothing  = return ()
    go (Just f) = liftIOE $ BSL.appendFile f bs


-- |
--
stdinLogging :: BSL.ByteString -> AppContext ()
stdinLogging bs = do
  stdioLogging $ str2lbs "[ IN]" `BSL.append` bs `BSL.append` str2lbs "\n"


-- |
--
stdoutLogging :: BSL.ByteString -> AppContext ()
stdoutLogging bs = do
  stdioLogging $ str2lbs "[OUT]" `BSL.append` bs `BSL.append` str2lbs "\n"



