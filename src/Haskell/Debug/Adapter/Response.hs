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

import qualified Haskell.DAP as DAP
import Haskell.Debug.Adapter.Type
import Haskell.Debug.Adapter.Utility
import Haskell.Debug.Adapter.Constant
import qualified Haskell.Debug.Adapter.MCP.Type as MCP

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

      lift (goApp res) >>= \case
        Nothing -> return ()
        Just bs -> yield bs
      
      res2lbs

  where
    goApp :: Response -> AppContext (Maybe B.ByteString)
    goApp res = view isMcpAppStores <$> get >>= \case
      False -> return . Just . encode $ res
      True  -> do
        let dapJson = encode res
            dapMsg = str2lbs "[INFO] dap response: " `B.append` dapJson
        mcpStderrLF dapMsg

        dap2mcpResponce res

    dap2mcpResponce :: Response -> AppContext (Maybe B.ByteString)
    dap2mcpResponce (McpInitializeResult res) = return . Just . encode $ res
    dap2mcpResponce (McpListToolsResult res) = return . Just . encode $ res
    dap2mcpResponce (InitializeResponse res) = mcpInitializeResponse res >>= return . Just
    dap2mcpResponce (InitializedEvent res) = mcpInitializedEvent res >>= return . Just
    dap2mcpResponce (SetBreakpointsResponse res) = mcpCallToolResponseWithData (DAP.request_seqSetBreakpointsResponse res) (encode (DAP.bodySetBreakpointsResponse res)) >>= return . Just
    dap2mcpResponce (ContinueResponse res) = mcpCallToolResponse (DAP.request_seqContinueResponse res) >>= return . Just
    dap2mcpResponce (StackTraceResponse res) = mcpStackTraceResponse res >>= return . Just
    dap2mcpResponce (ScopesResponse res) = mcpScopesResponse res >>= return . Just
    dap2mcpResponce (VariablesResponse res) = mcpVariablesResponse res >>= return . Just
    dap2mcpResponce (DisconnectResponse res) = mcpCallToolResponse (DAP.request_seqDisconnectResponse res) >>= return . Just
    dap2mcpResponce (TerminateResponse res) = mcpCallToolResponse (DAP.request_seqTerminateResponse res) >>= return . Just

    dap2mcpResponce _ = return Nothing


-- |
--
mcpCallToolResponseWithData :: Int -> B.ByteString -> AppContext B.ByteString
mcpCallToolResponseWithData idReq dataBS = do
  let dataBody = lbs2str dataBS
      answer = "{\"result\":\"success\",\"data\":" ++ dataBody ++ "}"
      

  let body = MCP.defaultMcpCallToolResultBody {
              MCP.contentMcpCallToolResultBody = [
                MCP.defaultMcpTextContent {
                  MCP.textMcpTextContent = answer
                }
              ]
            }
      res = MCP.defaultMcpCallToolResult {
            MCP.idMcpCallToolResult = idReq
          , MCP.resultMcpCallToolResult = body
          }

  return $ encode res

-- |
--
mcpCallToolResponse :: Int -> AppContext B.ByteString
mcpCallToolResponse idReq = do
  let answer = "success"

  let body = MCP.defaultMcpCallToolResultBody {
              MCP.contentMcpCallToolResultBody = [
                MCP.defaultMcpTextContent {
                  MCP.textMcpTextContent = answer
                }
              ]
            }
      res = MCP.defaultMcpCallToolResult {
            MCP.idMcpCallToolResult = idReq
          , MCP.resultMcpCallToolResult = body
          }

  return $ encode res


-- |
--
mcpInitializeResponse :: DAP.InitializeResponse -> AppContext B.ByteString
mcpInitializeResponse dapRes = do
  let answer = "success"

  let body = MCP.defaultMcpCallToolResultBody {
              MCP.contentMcpCallToolResultBody = [
                MCP.defaultMcpTextContent {
                  MCP.textMcpTextContent = answer
                }
              ]
            }
      res = MCP.defaultMcpCallToolResult {
            MCP.idMcpCallToolResult = DAP.request_seqInitializeResponse dapRes
          , MCP.resultMcpCallToolResult = body
          }

  return $ encode res


-- |
--
mcpInitializedEvent :: DAP.InitializedEvent -> AppContext B.ByteString
mcpInitializedEvent dapRes = do
  let answer = "success"

  let body = MCP.defaultMcpCallToolResultBody {
              MCP.contentMcpCallToolResultBody = [
                MCP.defaultMcpTextContent {
                  MCP.textMcpTextContent = answer
                }
              ]
            }
      res = MCP.defaultMcpCallToolResult {
            MCP.idMcpCallToolResult = DAP.request_seqInitializedEvent dapRes
          , MCP.resultMcpCallToolResult = body
          }

  return $ encode res


-- |
--
mcpSetBreakpointsResponse :: DAP.SetBreakpointsResponse -> AppContext B.ByteString
mcpSetBreakpointsResponse dapRes = do
  let answer = "success"

  let body = MCP.defaultMcpCallToolResultBody {
              MCP.contentMcpCallToolResultBody = [
                MCP.defaultMcpTextContent {
                  MCP.textMcpTextContent = answer
                }
              ]
            }
      res = MCP.defaultMcpCallToolResult {
            MCP.idMcpCallToolResult = DAP.request_seqSetBreakpointsResponse dapRes
          , MCP.resultMcpCallToolResult = body
          }

  return $ encode res

-- |
--
mcpStackTraceResponse :: DAP.StackTraceResponse -> AppContext B.ByteString
mcpStackTraceResponse dapRes = do
  let dataBody = lbs2str $ encode $ DAP.bodyStackTraceResponse dapRes
      answer = "{\"result\":\"success\",\"data\":" ++ dataBody ++ "}"
      

  let body = MCP.defaultMcpCallToolResultBody {
              MCP.contentMcpCallToolResultBody = [
                MCP.defaultMcpTextContent {
                  MCP.textMcpTextContent = answer
                }
              ]
            }
      res = MCP.defaultMcpCallToolResult {
            MCP.idMcpCallToolResult = DAP.request_seqStackTraceResponse dapRes
          , MCP.resultMcpCallToolResult = body
          }

  return $ encode res

-- |
--
mcpScopesResponse :: DAP.ScopesResponse -> AppContext B.ByteString
mcpScopesResponse dapRes = do
  let dataBody = lbs2str $ encode $ DAP.bodyScopesResponse dapRes
      answer = "{\"result\":\"success\",\"data\":" ++ dataBody ++ "}"
      

  let body = MCP.defaultMcpCallToolResultBody {
              MCP.contentMcpCallToolResultBody = [
                MCP.defaultMcpTextContent {
                  MCP.textMcpTextContent = answer
                }
              ]
            }
      res = MCP.defaultMcpCallToolResult {
            MCP.idMcpCallToolResult = DAP.request_seqScopesResponse dapRes
          , MCP.resultMcpCallToolResult = body
          }

  return $ encode res


-- |
--
mcpVariablesResponse :: DAP.VariablesResponse -> AppContext B.ByteString
mcpVariablesResponse dapRes = do
  let dataBody = lbs2str $ encode $ DAP.bodyVariablesResponse dapRes
      answer = "{\"result\":\"success\",\"data\":" ++ dataBody ++ "}"
      

  let body = MCP.defaultMcpCallToolResultBody {
              MCP.contentMcpCallToolResultBody = [
                MCP.defaultMcpTextContent {
                  MCP.textMcpTextContent = answer
                }
              ]
            }
      res = MCP.defaultMcpCallToolResult {
            MCP.idMcpCallToolResult = DAP.request_seqVariablesResponse dapRes
          , MCP.resultMcpCallToolResult = body
          }

  return $ encode res


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
      stdoutLogging bs
      view isMcpAppStores <$> get >>= \case
        False -> liftIO $ sendResponse wHdl bs
        True  -> liftIO $ mcpSendResponse wHdl bs

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


-- |
--
mcpSendResponse :: S.Handle -> B.ByteString -> IO ()
mcpSendResponse hdl str = do
  B.hPutStr hdl $ B.append str $ str2lbs _LF_STR
  S.hFlush hdl
