{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Haskell.Debug.Adapter.Control (
    ArgData(..)
  , run
  ) where

import System.IO
import qualified System.Log.Logger as L
import qualified Control.Exception.Safe as E
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Data.Version (showVersion)
import Paths_haskell_debug_adapter (version)
import Control.Lens

import Haskell.Debug.Adapter.Type
import Haskell.Debug.Adapter.Constant
import qualified Haskell.Debug.Adapter.Application as A
import qualified Haskell.Debug.Adapter.Request as RQ
import qualified Haskell.Debug.Adapter.Response as RP
import qualified Haskell.Debug.Adapter.Watch as W


-- |
--   Start HDA.
--   Default implementation is using STDIN/STDOUT handle.
--
--   Here is an example for using TCP Socket.
--
-- > import Network.Socket
-- >
-- > sock <- socket AF_INET Stream defaultProtocol
-- > let host = tupleToHostAddress (0, 0, 0, 0)
-- >     port = 9999
-- >     reqQ = 5
-- >
-- > bind sock $ SockAddrInet port host
-- > listen sock reqQ
-- >
-- > (conn, _) <- accept sock
-- > hdl <- socketToHandle conn ReadWriteMode
-- >
-- > run def hdl hdl
-- >
--
--   Port 9999 could be specified in the launch.json with "debugServer" attribute.
--
-- > "debugServer : 9999"
--
run :: ArgData -- ^command line arguments.
    -> Handle  -- ^IN handle. used to get request from the debug adapter client.
    -> Handle  -- ^OUT handle. used to response to the debug adapter client.
    -> IO ()
run argDat inHdl outHdl = E.bracket initialize finalize go

  where
    -- |
    --
    initialize = do
      L.debugM _LOG_NAME $ "initialize called."

      hSetBuffering inHdl NoBuffering
      hSetEncoding  inHdl utf8

      hSetBuffering outHdl NoBuffering
      hSetEncoding  outHdl utf8

      reqStore <- newMVar []
      resStore <- newMVar []
      evtStore <- newMVar []
      wsStore  <- newMVar ""
      logPRStore <- newMVar L.WARNING
      procStore  <- newEmptyMVar
      verStore   <- newEmptyMVar

      return AppStores {
        -- Read Only
          _appNameAppStores     = "haskell-debug-adapter"
        , _appVerAppStores      = showVersion version
        , _inHandleAppStores    = inHdl
        , _outHandleAppStores   = outHdl
        , _asyncsAppStores      = []
        , _stdioLogFileAppStores = argDat^.stdioLogFileArgData

        -- Read/Write from Application
        , _appStateWAppStores   = WrapAppState InitState
        , _resSeqAppStores      = 0
        , _startupAppStores     = ""
        , _startupFuncAppStores = ""
        , _startupArgsAppStores = ""
        , _stopOnEntryAppStores = False
        , _ghciPmptAppStores    = _GHCI_PROMPT_HDA
        , _mainArgsAppStores    = ""
        , _launchReqSeqAppStores = -1
        , _debugReRunableAppStores = False

        -- Read/Write ASync
        , _reqStoreAppStores    = reqStore
        , _resStoreAppStores    = resStore
        , _eventStoreAppStores  = evtStore
        , _workspaceAppStores   = wsStore
        , _logPriorityAppStores = logPRStore
        , _ghciProcAppStores    = procStore
        , _ghciVerAppStores     = verStore
        }

    -- |
    --
    finalize _ = do
      L.debugM _LOG_NAME $ "finalize called."
      L.removeAllHandlers

    -- |
    --
    go appData = do

      L.debugM _LOG_NAME $ "start thread manager."

      let ths = [
                  RQ.run appData    -- request handler
                , A.run  appData    -- main app
                , RP.run appData    -- response handler
                , W.run  appData    -- file watch
                ]

      as <- mapM async ths
      waitAnyCatchCancel as >>= \case
        (_, Right _) -> L.infoM _LOG_NAME $ "some threads stopped. exit."
        (_, Left e)  -> L.criticalM _LOG_NAME $ "some threads stopped. " ++ show e

