{-# LANGUAGE LambdaCase #-}

module Haskell.Debug.Adapter.Request where

import Control.Monad
import Control.Monad.IO.Class
import Data.Conduit
import Control.Lens
import Text.Parsec
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Control.Monad.State.Lazy
import qualified System.Log.Logger as L
import Control.Monad.Except
import Data.Maybe

import qualified Haskell.DAP as DAP
import Haskell.Debug.Adapter.Type
import Haskell.Debug.Adapter.Utility
import Haskell.Debug.Adapter.Constant
import qualified Haskell.Debug.Adapter.MCP.Type as MCP

-- |
--
run :: AppStores -> IO ()
run appData = do
  L.debugM _LOG_REQUEST "start request app"
  _ <- runApp appData app
  L.debugM _LOG_REQUEST "end request app"


-- |
--
app :: AppContext ()
app = flip catchError errHdl $ do
  _ <- runConduit pipeline
  return ()

  where
    pipeline :: ConduitM () Void AppContext ()
    pipeline = src .| lbs2req .| sink

    errHdl msg = do
      criticalEV _LOG_REQUEST msg
      addEvent CriticalExitEvent


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
    goApp = do
      bs <- view isMcpAppStores <$> get >>= \case
             True  -> mcpGetLine
             False -> getContentLength >>= getContent

      stdinLogging bs
      return bs


-- |
--
mcpGetLine :: AppContext B.ByteString
mcpGetLine = view inHandleAppStores <$> get >>= readLineL

-- |
--
getContent :: Int -> AppContext B.ByteString
getContent l = view inHandleAppStores <$> get
           >>= flip readCharsL l


-- |
--
getContentLength :: AppContext Int
getContentLength = go B.empty
  where
    go :: B.ByteString -> AppContext Int
    go buf = updateBuf buf >>= findLength

    updateBuf :: B.ByteString -> AppContext B.ByteString
    updateBuf buf = do
      hdl <- view inHandleAppStores <$> get
      B.append buf <$> readCharL hdl

    findLength :: B.ByteString -> AppContext Int
    findLength buf = case parse parser "find ContentLength parser" (lbs2str buf) of
      Left _  -> go buf
      Right l -> return l

    parser = do
      string _CONTENT_LENGTH
      len <- manyTill digit (string _TWO_CRLF)
      return . read $ len


---------------------------------------------------------------------------------
-- |
--
lbs2req :: ConduitT B.ByteString WrapRequest AppContext ()
lbs2req = do
  liftIO $ L.debugM _LOG_REQUEST $ "lbs2req start waiting."
  await >>= \case
    Nothing  -> do
      throwError $ "unexpectHed Nothing."
    Just reqBS -> do
      liftIO $ L.debugM _LOG_REQUEST $ "lbs2req get data. " ++ lbs2str reqBS
      lift (goApp reqBS) >>= \case
        Nothing -> return ()
        Just rq -> yield rq
      lbs2req

  where
    goApp :: B.ByteString -> AppContext (Maybe WrapRequest)
    goApp reqBS = flip catchError errHdl $ do
      isMcp <- view isMcpAppStores <$> get
      when isMcp $ mcpStderrLF $ str2lbs $ "[INFO] request: isMcp: " ++ (show isMcp) ++ ".\n"

      req <- view isMcpAppStores <$> get >>= \case
        False -> decodeRequest reqBS
        True  -> mcp2dapRequest <$> decodeMcpRequest reqBS

      when isMcp $ mcpStderrLF $ str2lbs $ "[INFO] mcp request: " ++ (show req) ++ ".\n"

      reqW <- createWrapRequest reqBS req
      return $ Just reqW

    -- |
    --
    errHdl msg = do
      warnEV _LOG_REQUEST msg
      return Nothing

-- |
--
decodeMcpRequest :: B.ByteString -> AppContext MCP.McpRequest
decodeMcpRequest bs = liftEither $ eitherDecode bs

-- |
--
mcp2dapRequest :: MCP.McpRequest -> DAP.Request
mcp2dapRequest req
  | "initialize" == MCP.methodMcpRequest req = DAP.Request {
                                                 seqRequest     = fromJust $ MCP.idMcpRequest req
                                               , typeRequest    = "request"
                                               , commandRequest = "mcp-" ++ (MCP.methodMcpRequest req)
                                               }
  | "notifications/initialized" == MCP.methodMcpRequest req = DAP.Request {
                                                 seqRequest     = 0
                                               , typeRequest    = "request"
                                               , commandRequest = "mcp-" ++ (MCP.methodMcpRequest req)
                                               }
  | "tools/list" == MCP.methodMcpRequest req = DAP.Request {
                                                 seqRequest     = fromJust $ MCP.idMcpRequest req
                                               , typeRequest    = "request"
                                               , commandRequest = "mcp-" ++ (MCP.methodMcpRequest req)
                                               }
  | "tools/call" == MCP.methodMcpRequest req = DAP.Request {
                                                 seqRequest     = fromJust $ MCP.idMcpRequest req
                                               , typeRequest    = "request"
                                               , commandRequest = "mcp-" ++ (MCP.methodMcpRequest req)
                                               }
  | otherwise = DAP.Request {
                              seqRequest     = fromJust $ MCP.idMcpRequest req
                            , typeRequest    = "request"
                            , commandRequest = "mcp-" ++ (MCP.methodMcpRequest req)
                            }
  
  
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
  | "pause" == DAP.commandRequest req = WrapRequest . PauseRequest <$> (liftEither (eitherDecode bs))
  | "terminate" == DAP.commandRequest req = WrapRequest . TerminateRequest <$> (liftEither (eitherDecode bs))
  | "setBreakpoints" == DAP.commandRequest req = WrapRequest . SetBreakpointsRequest <$> (liftEither (eitherDecode bs))
  | "setFunctionBreakpoints" == DAP.commandRequest req = WrapRequest . SetFunctionBreakpointsRequest <$> (liftEither (eitherDecode bs))
  | "setExceptionBreakpoints" == DAP.commandRequest req = WrapRequest . SetExceptionBreakpointsRequest <$> (liftEither (eitherDecode bs))
  | "configurationDone" == DAP.commandRequest req = WrapRequest . ConfigurationDoneRequest <$> (liftEither (eitherDecode bs))
  | "threads" == DAP.commandRequest req = WrapRequest . ThreadsRequest <$> (liftEither (eitherDecode bs))
  | "stackTrace" == DAP.commandRequest req = WrapRequest . StackTraceRequest <$> (liftEither (eitherDecode bs))
  | "scopes" == DAP.commandRequest req = WrapRequest . ScopesRequest <$> (liftEither (eitherDecode bs))
  | "variables" == DAP.commandRequest req = WrapRequest . VariablesRequest <$> (liftEither (eitherDecode bs))
  | "source" == DAP.commandRequest req = WrapRequest . SourceRequest <$> (liftEither (eitherDecode bs))
  | "continue" == DAP.commandRequest req = WrapRequest . ContinueRequest <$> (liftEither (eitherDecode bs))
  | "next" == DAP.commandRequest req = WrapRequest . NextRequest <$> (liftEither (eitherDecode bs))
  | "stepIn" == DAP.commandRequest req = WrapRequest . StepInRequest <$> (liftEither (eitherDecode bs))
  | "evaluate" == DAP.commandRequest req = WrapRequest . EvaluateRequest <$> (liftEither (eitherDecode bs))
  | "completions" == DAP.commandRequest req = WrapRequest . CompletionsRequest <$> (liftEither (eitherDecode bs))
  | "mcp-initialize" == DAP.commandRequest req = WrapRequest . McpInitializeRequest <$> (liftEither (eitherDecode bs))
  | "mcp-notifications/initialized" == DAP.commandRequest req = WrapRequest . McpInitializedNotification <$> (liftEither (eitherDecode bs))
  | "mcp-tools/list" == DAP.commandRequest req = WrapRequest . McpToolsListRequest <$> (liftEither (eitherDecode bs))
  | "mcp-tools/call" == DAP.commandRequest req = liftEither (eitherDecode bs) >>= createWrapRequestFromMCP
  | otherwise = throwError $ "unsupported request command. " ++ lbs2str bs


-- |
--
createWrapRequestFromMCP :: MCP.McpCallToolRequest -> AppContext WrapRequest
createWrapRequestFromMCP req
  | "dap-initialize" == getName = WrapRequest . InitializeRequest <$> createInitializeRequestFromMCP req
  | "dap-launch" == getName = WrapRequest . LaunchRequest <$> createLaunchRequestFromMCP req
  | "dap-set-breakpoints" == getName = WrapRequest . SetBreakpointsRequest <$> createSetBreakpointsRequestFromMCP req
  | "dap-continue" == getName = WrapRequest . ContinueRequest <$> createContinueRequestFromMCP req
  | "dap-stacktrace" == getName = WrapRequest . StackTraceRequest <$> createStackTraceRequestFromMCP req
  | "dap-scopes" == getName = WrapRequest . ScopesRequest <$> createScopesRequestFromMCP req
  | "dap-variables" == getName = WrapRequest . VariablesRequest <$> createVariablesRequestFromMCP req
  | "dap-disconnect" == getName = WrapRequest . DisconnectRequest <$> createDisconnectRequestFromMCP req
  | "dap-terminate" == getName = WrapRequest . TerminateRequest <$> createTerminateRequestFromMCP req
  | otherwise = throwError $ "unsupported mcp request command. " ++ show req
  where
    getName :: String
    getName = MCP.nameMcpCallToolRequestParams . MCP.paramsMcpCallToolRequest $ req

-- |
--
createInitializeRequestFromMCP :: MCP.McpCallToolRequest -> AppContext DAP.InitializeRequest
createInitializeRequestFromMCP src = do
  args <- liftEither $ eitherDecode $ str2lbs $ MCP.unRawJsonString $ MCP.argumentsMcpCallToolRequestParams $ MCP.paramsMcpCallToolRequest src

  let req = DAP.defaultInitializeRequest {
              DAP.seqInitializeRequest = MCP.idMcpCallToolRequest src
            , DAP.argumentsInitializeRequest = args
            }

  return req

-- |
--
createLaunchRequestFromMCP :: MCP.McpCallToolRequest -> AppContext DAP.LaunchRequest
createLaunchRequestFromMCP src = do
  args <- liftEither $ eitherDecode $ str2lbs $ MCP.unRawJsonString $ MCP.argumentsMcpCallToolRequestParams $ MCP.paramsMcpCallToolRequest src

  let req = DAP.defaultLaunchRequest {
              DAP.seqLaunchRequest = MCP.idMcpCallToolRequest src
            , DAP.argumentsLaunchRequest = args
            }

  return req


-- |
--
createSetBreakpointsRequestFromMCP :: MCP.McpCallToolRequest -> AppContext DAP.SetBreakpointsRequest
createSetBreakpointsRequestFromMCP src = do
  args <- liftEither $ eitherDecode $ str2lbs $ MCP.unRawJsonString $ MCP.argumentsMcpCallToolRequestParams $ MCP.paramsMcpCallToolRequest src

  let req = DAP.defaultSetBreakpointsRequest {
              DAP.seqSetBreakpointsRequest = MCP.idMcpCallToolRequest src
            , DAP.argumentsSetBreakpointsRequest = args
            }

  mcpStderrLF $ str2lbs $ "[INFO] createSetBreakpointsRequestFromMCP: req: " ++ (show req) ++ ".\n"
  return req


-- |
--
createContinueRequestFromMCP :: MCP.McpCallToolRequest -> AppContext DAP.ContinueRequest
createContinueRequestFromMCP src = do
  args <- liftEither $ eitherDecode $ str2lbs $ MCP.unRawJsonString $ MCP.argumentsMcpCallToolRequestParams $ MCP.paramsMcpCallToolRequest src

  let req = DAP.defaultContinueRequest {
              DAP.seqContinueRequest = MCP.idMcpCallToolRequest src
            , DAP.argumentsContinueRequest = args
            }

  mcpStderrLF $ str2lbs $ "[INFO] createContinueRequestFromMCP: req: " ++ (show req) ++ ".\n"
  return req


-- |
--
createStackTraceRequestFromMCP :: MCP.McpCallToolRequest -> AppContext DAP.StackTraceRequest
createStackTraceRequestFromMCP src = do
  args <- liftEither $ eitherDecode $ str2lbs $ MCP.unRawJsonString $ MCP.argumentsMcpCallToolRequestParams $ MCP.paramsMcpCallToolRequest src

  let req = DAP.defaultStackTraceRequest {
              DAP.seqStackTraceRequest = MCP.idMcpCallToolRequest src
            , DAP.argumentsStackTraceRequest = args
            }

  mcpStderrLF $ str2lbs $ "[INFO] createStackTraceRequestFromMCP: req: " ++ (show req) ++ ".\n"
  return req

-- |
--
createScopesRequestFromMCP :: MCP.McpCallToolRequest -> AppContext DAP.ScopesRequest
createScopesRequestFromMCP src = do
  args <- liftEither $ eitherDecode $ str2lbs $ MCP.unRawJsonString $ MCP.argumentsMcpCallToolRequestParams $ MCP.paramsMcpCallToolRequest src

  let req = DAP.defaultScopesRequest {
              DAP.seqScopesRequest = MCP.idMcpCallToolRequest src
            , DAP.argumentsScopesRequest = args
            }

  mcpStderrLF $ str2lbs $ "[INFO] createScopesRequestFromMCP: req: " ++ (show req) ++ ".\n"
  return req


-- |
--
createVariablesRequestFromMCP :: MCP.McpCallToolRequest -> AppContext DAP.VariablesRequest
createVariablesRequestFromMCP src = do
  args <- liftEither $ eitherDecode $ str2lbs $ MCP.unRawJsonString $ MCP.argumentsMcpCallToolRequestParams $ MCP.paramsMcpCallToolRequest src

  let req = DAP.defaultVariablesRequest {
              DAP.seqVariablesRequest = MCP.idMcpCallToolRequest src
            , DAP.argumentsVariablesRequest = args
            }

  mcpStderrLF $ str2lbs $ "[INFO] createVariablesRequestFromMCP: req: " ++ (show req) ++ ".\n"
  return req

-- |
--
createDisconnectRequestFromMCP :: MCP.McpCallToolRequest -> AppContext DAP.DisconnectRequest
createDisconnectRequestFromMCP src = do
  args <- liftEither $ eitherDecode $ str2lbs $ MCP.unRawJsonString $ MCP.argumentsMcpCallToolRequestParams $ MCP.paramsMcpCallToolRequest src

  let req = DAP.defaultDisconnectRequest {
              DAP.seqDisconnectRequest = MCP.idMcpCallToolRequest src
            , DAP.argumentsDisconnectRequest = args
            }

  mcpStderrLF $ str2lbs $ "[INFO] createDisconnectRequestFromMCP: req: " ++ (show req) ++ ".\n"
  return req
  
-- |
--
createTerminateRequestFromMCP :: MCP.McpCallToolRequest -> AppContext DAP.TerminateRequest
createTerminateRequestFromMCP src = do
  args <- liftEither $ eitherDecode $ str2lbs $ MCP.unRawJsonString $ MCP.argumentsMcpCallToolRequestParams $ MCP.paramsMcpCallToolRequest src

  let req = DAP.defaultTerminateRequest {
              DAP.seqTerminateRequest = MCP.idMcpCallToolRequest src
            , DAP.argumentsTerminateRequest = args
            }

  mcpStderrLF $ str2lbs $ "[INFO] createTerminateRequestFromMCP: req: " ++ (show req) ++ ".\n"
  return req




---------------------------------------------------------------------------------
-- |
--
sink :: ConduitT WrapRequest Void AppContext ()
sink = do
  liftIO $ L.debugM _LOG_REQUEST $ "sink start waiting."
  await >>= \case
    Nothing  -> do
      throwError $ "unexpected Nothing."
      return ()
    Just req -> do
      lift $ goApp req
      sink

  where
    -- |
    --
    goApp :: WrapRequest -> AppContext ()
    goApp = addRequest


