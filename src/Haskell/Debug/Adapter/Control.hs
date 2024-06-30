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
                -- , W.run  appData    -- file watch
                ]

      -- 
      -- suspend file watch.
      -- because on vbox CentOS Stream release 9, throw error.
      --   Error: couldn't start native file manager: fdType: unsupported operation (unknown file type)
      --
      -- https://github.com/haskell-fswatch/hfsnotify/blob/master/src/System/FSNotify.hs#L167C1-L178C7
      --   case confWatchMode conf of
      --     WatchModePoll interval -> WatchManager conf <$> liftIO (createPollManager interval) <*> cleanupVar <*> globalWatchChan
      -- #ifndef OS_BSD
      --     WatchModeOS -> liftIO (initSession ()) >>= createManager
      -- #endif
      -- 
      --   where
      -- #ifndef OS_BSD
      --     createManager :: Either Text NativeManager -> IO WatchManager
      --     createManager (Right nativeManager) = WatchManager conf nativeManager <$> cleanupVar <*> globalWatchChan
      --     createManager (Left err) = throwIO $ userError $ T.unpack $ "Error: couldn't start native file manager: " <> err
      -- #endif
      -- 
      -- https://github.com/haskell-fswatch/hfsnotify/blob/master/src/System/FSNotify/Linux.hs#L94C1-L97C45
      -- instance FileListener INotifyListener () where
      --   initSession _ = E.handle (\(e :: IOException) -> return $ Left $ fromString $ show e) $ do
      --     inotify <- INo.initINotify
      --     return $ Right $ INotifyListener inotify
      -- 
      -- https://hackage.haskell.org/package/hinotify-0.4.1/docs/src/System.INotify.html#initINotify
      -- initINotify :: IO INotify
      -- initINotify = do
      --     throwErrnoIfMinus1 "initINotify" c_inotify_init
      --     (fd,fd_type) <- FD.mkFD fdint ReadMode (Just (Stream,0,0))
      -- 




      as <- mapM async ths
      waitAnyCatchCancel as >>= \case
        (_, Right _) -> L.infoM _LOG_NAME $ "some threads stopped. exit."
        (_, Left e)  -> L.criticalM _LOG_NAME $ "some threads stopped. " ++ show e

