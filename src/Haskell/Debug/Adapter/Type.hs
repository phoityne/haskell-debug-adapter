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


--------------------------------------------------------------------------------
-- | Command Line Argument Data Type.
--
data ArgData = ArgData {
    _hackageVersionArgData :: Maybe String   -- ^deprecated.
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


$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "Source"} ''DAP.Source)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "SourceBreakpoint"} ''DAP.SourceBreakpoint)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "Breakpoint"} ''DAP.Breakpoint)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "FunctionBreakpoint"} ''DAP.FunctionBreakpoint)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "Thread"} ''DAP.Thread)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "StackFrame"} ''DAP.StackFrame)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "Scope"} ''DAP.Scope)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "Variable"} ''DAP.Variable)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "VariablePresentationHint"} ''DAP.VariablePresentationHint)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "CompletionsItem"} ''DAP.CompletionsItem)


-- jsonize
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "Request"} ''DAP.Request)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "InitializeRequest"} ''DAP.InitializeRequest)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "InitializeRequestArguments"} ''DAP.InitializeRequestArguments)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "LaunchRequest"} ''DAP.LaunchRequest)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "LaunchRequestArguments"} ''DAP.LaunchRequestArguments)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "DisconnectRequest"} ''DAP.DisconnectRequest)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "DisconnectRequestArguments"} ''DAP.DisconnectRequestArguments)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "PauseRequest"} ''DAP.PauseRequest)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "PauseRequestArguments"} ''DAP.PauseRequestArguments)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "TerminateRequest"} ''DAP.TerminateRequest)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "TerminateRequestArguments"} ''DAP.TerminateRequestArguments)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "SetBreakpointsRequest"} ''DAP.SetBreakpointsRequest)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "SetBreakpointsRequestArguments"} ''DAP.SetBreakpointsRequestArguments)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "SetFunctionBreakpointsRequest"} ''DAP.SetFunctionBreakpointsRequest)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "SetFunctionBreakpointsRequestArguments"} ''DAP.SetFunctionBreakpointsRequestArguments)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "SetExceptionBreakpointsRequest"} ''DAP.SetExceptionBreakpointsRequest)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "SetExceptionBreakpointsRequestArguments"} ''DAP.SetExceptionBreakpointsRequestArguments)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "ConfigurationDoneRequest"} ''DAP.ConfigurationDoneRequest)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "ThreadsRequest"} ''DAP.ThreadsRequest)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "StackTraceRequest"} ''DAP.StackTraceRequest)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "StackTraceRequestArguments"} ''DAP.StackTraceRequestArguments)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "ScopesRequest"} ''DAP.ScopesRequest)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "ScopesRequestArguments"} ''DAP.ScopesRequestArguments)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "VariablesRequest"} ''DAP.VariablesRequest)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "VariablesRequestArguments"} ''DAP.VariablesRequestArguments)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "ContinueRequest"} ''DAP.ContinueRequest)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "ContinueRequestArguments"} ''DAP.ContinueRequestArguments)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "NextRequest"} ''DAP.NextRequest)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "NextRequestArguments"} ''DAP.NextRequestArguments)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "StepInRequest"} ''DAP.StepInRequest)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "StepInRequestArguments"} ''DAP.StepInRequestArguments)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "EvaluateRequest"} ''DAP.EvaluateRequest)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "EvaluateRequestArguments"} ''DAP.EvaluateRequestArguments)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "CompletionsRequest"} ''DAP.CompletionsRequest)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "CompletionsRequestArguments"} ''DAP.CompletionsRequestArguments)


-- request
data Request a where
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
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "Response"} ''DAP.Response)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "ColumnDescriptor"} ''DAP.ColumnDescriptor)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "ExceptionBreakpointsFilter"} ''DAP.ExceptionBreakpointsFilter)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "InitializeResponse"} ''DAP.InitializeResponse)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "InitializeResponseBody"} ''DAP.InitializeResponseBody)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "LaunchResponse"} ''DAP.LaunchResponse)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "OutputEvent"} ''DAP.OutputEvent)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "OutputEventBody"} ''DAP.OutputEventBody)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "StoppedEvent"} ''DAP.StoppedEvent)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "StoppedEventBody"} ''DAP.StoppedEventBody)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "InitializedEvent"} ''DAP.InitializedEvent)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "DisconnectResponse"} ''DAP.DisconnectResponse)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "PauseResponse"} ''DAP.PauseResponse)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "TerminateResponse"} ''DAP.TerminateResponse)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "SetBreakpointsResponse"} ''DAP.SetBreakpointsResponse)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "SetBreakpointsResponseBody"} ''DAP.SetBreakpointsResponseBody)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "SetFunctionBreakpointsResponse"} ''DAP.SetFunctionBreakpointsResponse)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "SetFunctionBreakpointsResponseBody"} ''DAP.SetFunctionBreakpointsResponseBody)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "SetExceptionBreakpointsResponse"} ''DAP.SetExceptionBreakpointsResponse)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "ConfigurationDoneResponse"} ''DAP.ConfigurationDoneResponse)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "ThreadsResponse"} ''DAP.ThreadsResponse)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "ThreadsResponseBody"} ''DAP.ThreadsResponseBody)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "StackTraceResponse"} ''DAP.StackTraceResponse)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "StackTraceResponseBody"} ''DAP.StackTraceResponseBody)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "ScopesResponse"} ''DAP.ScopesResponse)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "ScopesResponseBody"} ''DAP.ScopesResponseBody)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "VariablesResponse"} ''DAP.VariablesResponse)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "VariablesResponseBody"} ''DAP.VariablesResponseBody)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "ContinueResponse"} ''DAP.ContinueResponse)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "NextResponse"} ''DAP.NextResponse)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "StepInResponse"} ''DAP.StepInResponse)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "EvaluateResponse"} ''DAP.EvaluateResponse)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "EvaluateResponseBody"} ''DAP.EvaluateResponseBody)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "CompletionsResponse"} ''DAP.CompletionsResponse)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "CompletionsResponseBody"} ''DAP.CompletionsResponseBody)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "TerminatedEvent"} ''DAP.TerminatedEvent)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "TerminatedEventBody"} ''DAP.TerminatedEventBody)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "ExitedEvent"} ''DAP.ExitedEvent)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "ExitedEventBody"} ''DAP.ExitedEventBody)

$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "ContinuedEvent"} ''DAP.ContinuedEvent)
$(deriveJSON defaultOptions {fieldLabelModifier = rdrop "ContinuedEventBody"} ''DAP.ContinuedEventBody)


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
  | ContinueResponse DAP.ContinueResponse
  | NextResponse DAP.NextResponse
  | StepInResponse DAP.StepInResponse
  | EvaluateResponse DAP.EvaluateResponse
  | CompletionsResponse DAP.CompletionsResponse
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


