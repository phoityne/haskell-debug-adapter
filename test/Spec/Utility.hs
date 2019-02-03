
module Spec.Utility where

import Control.Lens
import qualified System.IO as S
import qualified System.Process as S
import qualified Data.ByteString.Lazy as B
import Control.Concurrent.MVar
import qualified System.Log.Logger as L
import qualified System.Log.Formatter as L
import qualified System.Log.Handler as LH
import qualified System.Log.Handler.Simple as LHS

import Haskell.Debug.Adapter.Constant
import Haskell.Debug.Adapter.Utility
import Haskell.Debug.Adapter.Type


-- |
--  setup logger
-- 
setUpLogger :: L.Priority -> IO ()
setUpLogger p = do
  logHandle <- LHS.streamHandler S.stdout p
  
  let logFormat  = L.tfLogFormatter _LOG_FORMAT_DATE _LOG_FORMAT
      logHandler = LH.setFormatter logHandle logFormat

  L.updateGlobalLogger L.rootLoggerName $ L.setHandlers ([] :: [LHS.GenericHandler S.Handle])
  L.updateGlobalLogger _LOG_NAME $ L.setHandlers [logHandler]
  L.updateGlobalLogger _LOG_NAME $ L.setLevel p

  L.updateGlobalLogger _LOG_REQUEST $ L.setHandlers [logHandler]
  L.updateGlobalLogger _LOG_REQUEST $ L.setLevel p

  L.updateGlobalLogger _LOG_RESPONSE $ L.setHandlers [logHandler]
  L.updateGlobalLogger _LOG_RESPONSE $ L.setLevel p

  L.updateGlobalLogger _LOG_APP $ L.setHandlers [logHandler]
  L.updateGlobalLogger _LOG_APP $ L.setLevel p

  L.updateGlobalLogger _LOG_THREAD_MGR $ L.setHandlers [logHandler]
  L.updateGlobalLogger _LOG_THREAD_MGR $ L.setLevel p

  L.updateGlobalLogger _LOG_GHCI_STDOUT $ L.setHandlers [logHandler]
  L.updateGlobalLogger _LOG_GHCI_STDOUT $ L.setLevel p

  L.updateGlobalLogger _LOG_WATCH $ L.setHandlers [logHandler]
  L.updateGlobalLogger _LOG_WATCH $ L.setLevel p


-- |
--  setup logger
-- 
tearDownLogger :: IO ()
tearDownLogger = do
  L.removeAllHandlers


-- |
--
createPipe :: IO (S.Handle, S.Handle)
createPipe = do
  (rHdl, wHdl) <- S.createPipe

  S.hSetBuffering rHdl S.NoBuffering
  S.hSetEncoding rHdl S.utf8
  S.hSetNewlineMode rHdl $ S.NewlineMode S.CRLF S.LF

  S.hSetBuffering wHdl S.NoBuffering
  S.hSetEncoding wHdl S.utf8
  S.hSetNewlineMode wHdl $ S.NewlineMode S.CRLF S.LF

  return (rHdl, wHdl) 


-- |
--
read2buf :: S.Handle -> MVar B.ByteString -> IO ()
read2buf hdl bufMVar = do
  s <- B.hGet hdl 1
  buf <- takeMVar bufMVar
  putMVar bufMVar $ buf `B.append` s
  read2buf hdl bufMVar

