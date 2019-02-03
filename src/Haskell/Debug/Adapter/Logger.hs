module Haskell.Debug.Adapter.Logger where

import Haskell.Debug.Adapter.Constant

import System.IO
import qualified System.Log.Logger as L
import qualified System.Log.Formatter as L
import qualified System.Log.Handler as LH
import qualified System.Log.Handler.Simple as LHS


-- |
--  setup logger
-- 
setUpLogger :: FilePath -> L.Priority -> IO ()
setUpLogger path lv = do

  L.removeAllHandlers
  
  logStream <- openFile path AppendMode
  hSetEncoding logStream utf8
  hSetBuffering logStream NoBuffering

  logH <- LHS.streamHandler logStream lv

  let logHandle  = logH {LHS.closeFunc = hClose}
      logFormat  = L.tfLogFormatter _LOG_FORMAT_DATE _LOG_FORMAT
      logHandler = LH.setFormatter logHandle logFormat

  L.updateGlobalLogger L.rootLoggerName $ L.setHandlers ([] :: [LHS.GenericHandler Handle])

  L.updateGlobalLogger _LOG_NAME $ L.setHandlers [logHandler]
  L.updateGlobalLogger _LOG_NAME $ L.setLevel lv

  L.updateGlobalLogger _LOG_REQUEST $ L.setHandlers [logHandler]
  L.updateGlobalLogger _LOG_REQUEST $ L.setLevel lv

  L.updateGlobalLogger _LOG_RESPONSE $ L.setHandlers [logHandler]
  L.updateGlobalLogger _LOG_RESPONSE $ L.setLevel lv

  L.updateGlobalLogger _LOG_APP $ L.setHandlers [logHandler]
  L.updateGlobalLogger _LOG_APP $ L.setLevel lv

  L.updateGlobalLogger _LOG_THREAD_MGR $ L.setHandlers [logHandler]
  L.updateGlobalLogger _LOG_THREAD_MGR $ L.setLevel lv

  L.updateGlobalLogger _LOG_EVENT $ L.setHandlers [logHandler]
  L.updateGlobalLogger _LOG_EVENT $ L.setLevel lv

  L.updateGlobalLogger _LOG_GHCI_STDOUT $ L.setHandlers [logHandler]
  L.updateGlobalLogger _LOG_GHCI_STDOUT $ L.setLevel lv

  L.updateGlobalLogger _LOG_WATCH $ L.setHandlers [logHandler]
  L.updateGlobalLogger _LOG_WATCH $ L.setLevel lv


