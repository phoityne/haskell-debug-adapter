{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

module Haskell.Debug.Adapter.Type where

import Data.Data
import Data.Default
import Control.Lens
import Data.Aeson
import Data.Aeson.TH
import Control.Monad.Except
import Control.Monad.State
import Control.Concurrent
import Control.Concurrent.Async
import qualified System.IO as S
import qualified Data.Text as T
import qualified System.Log.Logger as L
import qualified System.Process as S
import qualified Data.Version as V

import qualified Haskell.DAP as DAP
import Haskell.Debug.Adapter.TH.Utility
import Haskell.Debug.Adapter.Constant
import qualified Data.ByteString.Lazy.Char8 as BL

import qualified Haskell.Debug.Adapter.MCP.Type as MCP

--------------------------------------------------------------------------------
-- | Command Line Argument Data Type.
--
data ArgData = ArgData {
    _hackageVersionArgData :: Maybe String   -- ^deprecated.
  , _stdioLogFileArgData   :: Maybe FilePath -- stdio log file.
  , _mcpArgData            :: Bool           -- run as mcp server.
  } deriving (Data, Typeable, Show, Read, Eq)

makeLenses ''ArgData
$(deriveJSON
  defaultOptions {
      fieldLabelModifier = fieldModifier "ArgData"
    }
  ''ArgData)


-- |
--   default value instance.
--
instance Default ArgData where
  def = ArgData {
        _hackageVersionArgData = Nothing
      , _stdioLogFileArgData   = Nothing
      , _mcpArgData            = False
      }


--------------------------------------------------------------------------------
-- |
--
instance FromJSON  L.Priority  where
  parseJSON (String v) = pure $ read $ T.unpack v
  parseJSON o = error $ "json parse error. Priority:" ++ show o

instance ToJSON L.Priority  where
  toJSON (L.DEBUG)     = String $ T.pack "DEBUG"
  toJSON (L.INFO)      = String $ T.pack "INFO"
  toJSON (L.NOTICE)    = String $ T.pack "NOTICE"
  toJSON (L.WARNING)   = String $ T.pack "WARNING"
  toJSON (L.ERROR)     = String $ T.pack "ERROR"
  toJSON (L.CRITICAL)  = String $ T.pack "CRITICAL"
  toJSON (L.ALERT)     = String $ T.pack "ALERT"
  toJSON (L.EMERGENCY) = String $ T.pack "EMERGENCY"

--------------------------------------------------------------------------------
-- | Config Data
--
data ConfigData = ConfigData {
    _workDirConfigData  :: FilePath
  , _logFileConfigData  :: FilePath
  , _logLevelConfigData :: L.Priority
  } deriving (Show, Read, Eq)

makeLenses ''ConfigData

instance Default ConfigData where
  def = ConfigData {
        _workDirConfigData  = "."
      , _logFileConfigData  = "haskell-debug-adapter.log"
      , _logLevelConfigData = L.WARNING
      }

$(deriveJSON defaultOptions { fieldLabelModifier = fieldModifier "ConfigData" } ''ConfigData)



--------------------------------------------------------------------------------------

data StateTransit =
    Init_GHCiRun
  | Init_Shutdown
  | GHCiRun_DebugRun
  | GHCiRun_Contaminated
  | GHCiRun_Shutdown
  | DebugRun_Contaminated
  | DebugRun_Shutdown
  | DebugRun_GHCiRun
  | Contaminated_Shutdown
  deriving (Show, Read, Eq)

$(deriveJSON defaultOptions ''StateTransit)

--------------------------------------------------------------------------------------

data HdaInternalTransitRequest = HdaInternalTransitRequest {
    stateHdaInternalTransitRequest :: StateTransit
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = fieldModifier "HdaInternalTransitRequest" } ''HdaInternalTransitRequest)

data HdaInternalTerminateRequest = HdaInternalTerminateRequest {
    msgHdaInternalTerminateRequest :: String
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = fieldModifier "HdaInternalTerminateRequest" } ''HdaInternalTerminateRequest)

data HdaInternalLoadRequest = HdaInternalLoadRequest {
    pathHdaInternalLoadRequest :: FilePath
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = fieldModifier "HdaInternalLoadRequest" } ''HdaInternalLoadRequest)

--------------------------------------------------------------------------------
-- | DAP Request Data
--


$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "Source", omitNothingFields = True} ''DAP.Source)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "SourceBreakpoint", omitNothingFields = True} ''DAP.SourceBreakpoint)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "Breakpoint", omitNothingFields = True} ''DAP.Breakpoint)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "FunctionBreakpoint", omitNothingFields = True} ''DAP.FunctionBreakpoint)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "Thread", omitNothingFields = True} ''DAP.Thread)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "StackFrame", omitNothingFields = True} ''DAP.StackFrame)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "Scope", omitNothingFields = True} ''DAP.Scope)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "VariablePresentationHint", omitNothingFields = True} ''DAP.VariablePresentationHint)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "Variable", omitNothingFields = True} ''DAP.Variable)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "CompletionsItem", omitNothingFields = True} ''DAP.CompletionsItem)





instance FromJSON MCP.RawJsonString where
  parseJSON v = pure . MCP.RawJsonString . BL.unpack $ encode v
instance ToJSON MCP.RawJsonString where
  toJSON (MCP.RawJsonString str) =
    case decode (BL.pack str) of
      Just v  -> v
      Nothing -> error "Invalid JSON string in RawJsonString"

-- jsonize
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "McpRequest", omitNothingFields = True} ''MCP.McpRequest)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "McpInitializeRequest", omitNothingFields = True} ''MCP.McpInitializeRequest)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "McpInitializedNotification", omitNothingFields = True} ''MCP.McpInitializedNotification)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "McpToolsListRequest", omitNothingFields = True} ''MCP.McpToolsListRequest)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "McpCallToolRequestParams", omitNothingFields = True} ''MCP.McpCallToolRequestParams)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "McpCallToolRequest", omitNothingFields = True} ''MCP.McpCallToolRequest)







$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "Request", omitNothingFields = True} ''DAP.Request)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "InitializeRequestArguments", omitNothingFields = True} ''DAP.InitializeRequestArguments)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "InitializeRequest", omitNothingFields = True} ''DAP.InitializeRequest)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "LaunchRequestArguments", omitNothingFields = True} ''DAP.LaunchRequestArguments)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "LaunchRequest", omitNothingFields = True} ''DAP.LaunchRequest)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "DisconnectRequestArguments", omitNothingFields = True} ''DAP.DisconnectRequestArguments)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "DisconnectRequest", omitNothingFields = True} ''DAP.DisconnectRequest)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "PauseRequestArguments", omitNothingFields = True} ''DAP.PauseRequestArguments)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "PauseRequest", omitNothingFields = True} ''DAP.PauseRequest)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "TerminateRequestArguments", omitNothingFields = True} ''DAP.TerminateRequestArguments)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "TerminateRequest", omitNothingFields = True} ''DAP.TerminateRequest)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "SetBreakpointsRequestArguments", omitNothingFields = True} ''DAP.SetBreakpointsRequestArguments)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "SetBreakpointsRequest", omitNothingFields = True} ''DAP.SetBreakpointsRequest)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "SetFunctionBreakpointsRequestArguments", omitNothingFields = True} ''DAP.SetFunctionBreakpointsRequestArguments)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "SetFunctionBreakpointsRequest", omitNothingFields = True} ''DAP.SetFunctionBreakpointsRequest)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "SetExceptionBreakpointsRequestArguments", omitNothingFields = True} ''DAP.SetExceptionBreakpointsRequestArguments)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "SetExceptionBreakpointsRequest", omitNothingFields = True} ''DAP.SetExceptionBreakpointsRequest)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "ConfigurationDoneRequest", omitNothingFields = True} ''DAP.ConfigurationDoneRequest)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "ThreadsRequest", omitNothingFields = True} ''DAP.ThreadsRequest)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "StackTraceRequestArguments", omitNothingFields = True} ''DAP.StackTraceRequestArguments)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "StackTraceRequest", omitNothingFields = True} ''DAP.StackTraceRequest)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "ScopesRequestArguments", omitNothingFields = True} ''DAP.ScopesRequestArguments)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "ScopesRequest", omitNothingFields = True} ''DAP.ScopesRequest)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "VariablesRequestArguments", omitNothingFields = True} ''DAP.VariablesRequestArguments)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "VariablesRequest", omitNothingFields = True} ''DAP.VariablesRequest)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "SourceRequestArguments", omitNothingFields = True} ''DAP.SourceRequestArguments)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "SourceRequest", omitNothingFields = True} ''DAP.SourceRequest)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "ContinueRequestArguments", omitNothingFields = True} ''DAP.ContinueRequestArguments)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "ContinueRequest", omitNothingFields = True} ''DAP.ContinueRequest)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "NextRequestArguments", omitNothingFields = True} ''DAP.NextRequestArguments)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "NextRequest", omitNothingFields = True} ''DAP.NextRequest)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "StepInRequestArguments", omitNothingFields = True} ''DAP.StepInRequestArguments)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "StepInRequest", omitNothingFields = True} ''DAP.StepInRequest)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "EvaluateRequestArguments", omitNothingFields = True} ''DAP.EvaluateRequestArguments)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "EvaluateRequest", omitNothingFields = True} ''DAP.EvaluateRequest)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "CompletionsRequestArguments", omitNothingFields = True} ''DAP.CompletionsRequestArguments)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "CompletionsRequest", omitNothingFields = True} ''DAP.CompletionsRequest)


-- request
data Request a where
  McpInitializeRequest :: MCP.McpInitializeRequest -> Request MCP.McpInitializeRequest
  McpInitializedNotification :: MCP.McpInitializedNotification -> Request MCP.McpInitializedNotification
  McpToolsListRequest :: MCP.McpToolsListRequest -> Request MCP.McpToolsListRequest
  McpCallToolRequest :: MCP.McpCallToolRequest -> Request MCP.McpCallToolRequest
  
  
  InitializeRequest :: DAP.InitializeRequest -> Request DAP.InitializeRequest
  LaunchRequest     :: DAP.LaunchRequest     -> Request DAP.LaunchRequest
  DisconnectRequest :: DAP.DisconnectRequest -> Request DAP.DisconnectRequest
  PauseRequest :: DAP.PauseRequest -> Request DAP.PauseRequest
  TerminateRequest :: DAP.TerminateRequest -> Request DAP.TerminateRequest
  SetBreakpointsRequest :: DAP.SetBreakpointsRequest -> Request DAP.SetBreakpointsRequest
  SetFunctionBreakpointsRequest :: DAP.SetFunctionBreakpointsRequest -> Request DAP.SetFunctionBreakpointsRequest
  SetExceptionBreakpointsRequest :: DAP.SetExceptionBreakpointsRequest -> Request DAP.SetExceptionBreakpointsRequest
  ConfigurationDoneRequest :: DAP.ConfigurationDoneRequest -> Request DAP.ConfigurationDoneRequest
  ThreadsRequest :: DAP.ThreadsRequest -> Request DAP.ThreadsRequest
  StackTraceRequest :: DAP.StackTraceRequest -> Request DAP.StackTraceRequest
  ScopesRequest :: DAP.ScopesRequest -> Request DAP.ScopesRequest
  VariablesRequest :: DAP.VariablesRequest -> Request DAP.VariablesRequest
  SourceRequest :: DAP.SourceRequest -> Request DAP.SourceRequest
  ContinueRequest :: DAP.ContinueRequest -> Request DAP.ContinueRequest
  NextRequest :: DAP.NextRequest -> Request DAP.NextRequest
  StepInRequest :: DAP.StepInRequest -> Request DAP.StepInRequest
  EvaluateRequest :: DAP.EvaluateRequest -> Request DAP.EvaluateRequest
  CompletionsRequest :: DAP.CompletionsRequest -> Request DAP.CompletionsRequest
  InternalTransitRequest :: HdaInternalTransitRequest -> Request HdaInternalTransitRequest
  InternalTerminateRequest :: HdaInternalTerminateRequest -> Request HdaInternalTerminateRequest
  InternalLoadRequest :: HdaInternalLoadRequest -> Request HdaInternalLoadRequest

deriving instance Show r => Show (Request r)

data WrapRequest = forall a. WrapRequest (Request a)

--------------------------------------------------------------------------------
-- | DAP Response Data
--


-- jsonize
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "McpServerCapabilitiesTools", omitNothingFields = True} ''MCP.McpServerCapabilitiesTools)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "McpServerCapabilities", omitNothingFields = True} ''MCP.McpServerCapabilities)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "McpImplementation", omitNothingFields = True} ''MCP.McpImplementation)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "McpInitializeResultBody", omitNothingFields = True} ''MCP.McpInitializeResultBody)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "McpInitializeResult", omitNothingFields = True} ''MCP.McpInitializeResult)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "McpKeyType", omitNothingFields = True} ''MCP.McpKeyType)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "McpInputSchemaProperties", omitNothingFields = True, sumEncoding = UntaggedValue} ''MCP.McpInputSchemaProperties)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "McpInputSchema", omitNothingFields = True} ''MCP.McpInputSchema)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "McpTool", omitNothingFields = True} ''MCP.McpTool)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "McpListToolsResultBody", omitNothingFields = True} ''MCP.McpListToolsResultBody)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "McpListToolsResult", omitNothingFields = True} ''MCP.McpListToolsResult)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "McpTextContent", omitNothingFields = True} ''MCP.McpTextContent)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "McpCallToolResultBody", omitNothingFields = True} ''MCP.McpCallToolResultBody)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "McpCallToolResult", omitNothingFields = True} ''MCP.McpCallToolResult)



$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "Response", omitNothingFields = True} ''DAP.Response)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "ColumnDescriptor", omitNothingFields = True} ''DAP.ColumnDescriptor)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "ExceptionBreakpointsFilter", omitNothingFields = True} ''DAP.ExceptionBreakpointsFilter)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "InitializeResponseBody", omitNothingFields = True} ''DAP.InitializeResponseBody)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "InitializeResponse", omitNothingFields = True} ''DAP.InitializeResponse)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "LaunchResponse", omitNothingFields = True} ''DAP.LaunchResponse)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "OutputEventBody", omitNothingFields = True} ''DAP.OutputEventBody)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "OutputEvent", omitNothingFields = True} ''DAP.OutputEvent)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "StoppedEventBody", omitNothingFields = True} ''DAP.StoppedEventBody)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "StoppedEvent", omitNothingFields = True} ''DAP.StoppedEvent)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "InitializedEvent", omitNothingFields = True} ''DAP.InitializedEvent)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "DisconnectResponse", omitNothingFields = True} ''DAP.DisconnectResponse)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "PauseResponse", omitNothingFields = True} ''DAP.PauseResponse)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "TerminateResponse", omitNothingFields = True} ''DAP.TerminateResponse)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "SetBreakpointsResponseBody", omitNothingFields = True} ''DAP.SetBreakpointsResponseBody)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "SetBreakpointsResponse", omitNothingFields = True} ''DAP.SetBreakpointsResponse)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "SetFunctionBreakpointsResponseBody", omitNothingFields = True} ''DAP.SetFunctionBreakpointsResponseBody)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "SetFunctionBreakpointsResponse", omitNothingFields = True} ''DAP.SetFunctionBreakpointsResponse)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "SetExceptionBreakpointsResponse", omitNothingFields = True} ''DAP.SetExceptionBreakpointsResponse)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "ConfigurationDoneResponse", omitNothingFields = True} ''DAP.ConfigurationDoneResponse)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "ThreadsResponseBody", omitNothingFields = True} ''DAP.ThreadsResponseBody)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "ThreadsResponse", omitNothingFields = True} ''DAP.ThreadsResponse)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "StackTraceResponseBody", omitNothingFields = True} ''DAP.StackTraceResponseBody)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "StackTraceResponse", omitNothingFields = True} ''DAP.StackTraceResponse)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "ScopesResponseBody", omitNothingFields = True} ''DAP.ScopesResponseBody)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "ScopesResponse", omitNothingFields = True} ''DAP.ScopesResponse)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "VariablesResponseBody", omitNothingFields = True} ''DAP.VariablesResponseBody)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "VariablesResponse", omitNothingFields = True} ''DAP.VariablesResponse)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "SourceResponseBody", omitNothingFields = True} ''DAP.SourceResponseBody)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "SourceResponse", omitNothingFields = True} ''DAP.SourceResponse)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "ContinueResponse", omitNothingFields = True} ''DAP.ContinueResponse)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "NextResponse", omitNothingFields = True} ''DAP.NextResponse)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "StepInResponse", omitNothingFields = True} ''DAP.StepInResponse)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "EvaluateResponseBody", omitNothingFields = True} ''DAP.EvaluateResponseBody)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "EvaluateResponse", omitNothingFields = True} ''DAP.EvaluateResponse)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "CompletionsResponseBody", omitNothingFields = True} ''DAP.CompletionsResponseBody)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "CompletionsResponse", omitNothingFields = True} ''DAP.CompletionsResponse)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "TerminatedEventBody", omitNothingFields = True} ''DAP.TerminatedEventBody)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "TerminatedEvent", omitNothingFields = True} ''DAP.TerminatedEvent)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "ExitedEventBody", omitNothingFields = True} ''DAP.ExitedEventBody)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "ExitedEvent", omitNothingFields = True} ''DAP.ExitedEvent)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "ContinuedEventBody", omitNothingFields = True} ''DAP.ContinuedEventBody)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "ContinuedEvent", omitNothingFields = True} ''DAP.ContinuedEvent)


-- response
data Response =
    InitializeResponse DAP.InitializeResponse
  | LaunchResponse     DAP.LaunchResponse
  | OutputEvent        DAP.OutputEvent
  | StoppedEvent       DAP.StoppedEvent
  | TerminatedEvent    DAP.TerminatedEvent
  | ExitedEvent        DAP.ExitedEvent
  | ContinuedEvent     DAP.ContinuedEvent
  | InitializedEvent   DAP.InitializedEvent
  | DisconnectResponse DAP.DisconnectResponse
  | PauseResponse DAP.PauseResponse
  | TerminateResponse DAP.TerminateResponse
  | SetBreakpointsResponse DAP.SetBreakpointsResponse
  | SetFunctionBreakpointsResponse DAP.SetFunctionBreakpointsResponse
  | SetExceptionBreakpointsResponse DAP.SetExceptionBreakpointsResponse
  | ConfigurationDoneResponse DAP.ConfigurationDoneResponse
  | ThreadsResponse DAP.ThreadsResponse
  | StackTraceResponse DAP.StackTraceResponse
  | ScopesResponse DAP.ScopesResponse
  | VariablesResponse DAP.VariablesResponse
  | SourceResponse DAP.SourceResponse
  | ContinueResponse DAP.ContinueResponse
  | NextResponse DAP.NextResponse
  | StepInResponse DAP.StepInResponse
  | EvaluateResponse DAP.EvaluateResponse
  | CompletionsResponse DAP.CompletionsResponse
  | McpInitializeResult MCP.McpInitializeResult
  | McpListToolsResult MCP.McpListToolsResult
  | McpCallToolResult MCP.McpCallToolResult
  deriving (Show, Read, Eq)

$(deriveJSON defaultOptions{sumEncoding = UntaggedValue} ''Response)

--------------------------------------------------------------------------------
-- | State
--
data InitStateData         = InitStateData deriving (Show, Eq)
data GHCiRunStateData      = GHCiRunStateData deriving (Show, Eq)
data DebugRunStateData     = DebugRunStateData deriving (Show, Eq)
data ContaminatedStateData = ContaminatedStateData deriving (Show, Eq)
data ShutdownStateData     = ShutdownStateData deriving (Show, Eq)

data AppState s where
  InitState     :: AppState InitStateData
  GHCiRunState  :: AppState GHCiRunStateData
  DebugRunState :: AppState DebugRunStateData
  ShutdownState :: AppState ShutdownStateData
  ContaminatedState :: AppState ContaminatedStateData

deriving instance Show s => Show (AppState s)

class AppStateIF s where
  entryAction :: (AppState s) -> AppContext ()
  exitAction  :: (AppState s) -> AppContext ()
  doActivity  :: (AppState s) -> WrapRequest -> AppContext (Maybe StateTransit)

data WrapAppState = forall s. (AppStateIF s) => WrapAppState (AppState s)

class WrapAppStateIF s where
  entryActionW :: s -> AppContext ()
  exitActionW  :: s -> AppContext ()
  doActivityW  :: s -> WrapRequest -> AppContext (Maybe StateTransit)

instance WrapAppStateIF WrapAppState where
  entryActionW (WrapAppState s) = entryAction s
  exitActionW  (WrapAppState s) = exitAction s
  doActivityW  (WrapAppState s) r = doActivity s r

-- |
--
class  (Show s, Show r) => StateActivityIF s r where
  action :: (AppState s) -> (Request r) -> AppContext (Maybe StateTransit)
  --action _ _ = return Nothing
  action s r = do
    liftIO $ L.warningM _LOG_APP $ show s ++ " " ++ show r ++ " not supported. nop."
    return Nothing

--------------------------------------------------------------------------------
-- | Event
--
data Event =
  CriticalExitEvent
  deriving (Show, Read, Eq)


--------------------------------------------------------------------------------
-- |
--
data GHCiProc = GHCiProc {
    _wHdLGHCiProc :: S.Handle
  , _rHdlGHCiProc :: S.Handle
  , _errGHCiProc  :: S.Handle
  , _procGHCiProc :: S.ProcessHandle
  }


--------------------------------------------------------------------------------
-- | Application Context
--

type ErrMsg = String
type AppContext = StateT AppStores (ExceptT ErrMsg IO)


-- | Application Context Data
--
data AppStores = AppStores {
  -- Read Only
    _appNameAppStores     :: String
  , _appVerAppStores      :: String
  , _inHandleAppStores    :: S.Handle
  , _outHandleAppStores   :: S.Handle
  , _asyncsAppStores      :: [Async ()]
  , _stdioLogFileAppStores :: Maybe FilePath
  , _isMcpAppStores          :: Bool

  -- Read/Write from Application
  , _appStateWAppStores   :: WrapAppState
  , _resSeqAppStores      :: Int
  , _startupAppStores     :: FilePath
  , _startupFuncAppStores :: String
  , _startupArgsAppStores :: String
  , _stopOnEntryAppStores :: Bool
  , _ghciPmptAppStores    :: String
  , _mainArgsAppStores    :: String
  , _launchReqSeqAppStores :: Int
  , _debugReRunableAppStores :: Bool

  -- Global Read/Write ASync
  , _reqStoreAppStores    :: MVar [WrapRequest]
  , _resStoreAppStores    :: MVar [Response]
  , _eventStoreAppStores  :: MVar [Event]
  , _workspaceAppStores   :: MVar FilePath
  , _logPriorityAppStores :: MVar L.Priority
  , _ghciProcAppStores    :: MVar GHCiProc
  --, _ghciStdoutAppStores  :: MVar B.ByteString
  , _ghciVerAppStores     :: MVar V.Version
  }

makeLenses ''AppStores
makeLenses ''GHCiProc


