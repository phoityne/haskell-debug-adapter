{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable  #-}

module Phoityne.VSCode.Control (
  run
) where

import System.IO
import Control.Concurrent
import Text.Parsec
import Control.Lens
import qualified System.Log.Logger as L

import Phoityne.VSCode.Constant
import Phoityne.VSCode.Utility
import Phoityne.VSCode.Type
import qualified Phoityne.VSCode.Argument as A
import qualified Phoityne.VSCode.Core as C
import qualified Data.ByteString.Lazy as BSL
import System.Exit
import qualified Control.Exception.Safe as E


-- |
-- 
_CONTENT_LENGTH :: String
_CONTENT_LENGTH = "Content-Length: " 


-- |
-- 
run :: IO Int
run = flip E.catches handlers $
  E.finally go finalize

  where
    handlers = [ E.Handler helpExcept
               , E.Handler exitExcept
               , E.Handler ioExcept
               , E.Handler someExcept
               ]
    finalize = L.removeAllHandlers 
    helpExcept (_ :: A.HelpExitException) = return 0 
    exitExcept ExitSuccess                = return 0
    exitExcept (ExitFailure c)            = return c
    ioExcept   (e :: E.IOException)       = print e >> return 1
    someExcept (e :: E.SomeException)     = print e >> return 1


-- |
-- 
go :: IO Int
go = do

  args <- A.getArgData

  hSetBuffering stdin NoBuffering
  hSetEncoding  stdin utf8

  hSetBuffering stdout NoBuffering
  hSetEncoding  stdout utf8

  mvarDat <- newMVar C.defaultDebugContextData {
                       C.responseHandlerDebugContextData = sendResponse
                     , C.hackagePackageVersionDebugContextData = args ^. hackageVersionArgData
                     }

  wait mvarDat

  return 1


-- |
--
-- 
wait :: MVar C.DebugContextData -> IO ()
wait mvarDat = readStdin BSL.empty
  where
    readStdin buf = BSL.hGet stdin 1 >>= withC buf
    
    withC buf c
      | c == BSL.empty = unexpectedEOF
      | otherwise      = withBuf $ BSL.append buf c
    
    withBuf buf = case parse parser "readContentLengthParser" (lbs2str buf) of
        Left _    -> readStdin buf
        Right len -> BSL.hGet stdin len >>= withCnt buf

    withCnt buf cnt
      | cnt == BSL.empty = unexpectedEOF
      | otherwise        = do
        C.handleRequest mvarDat buf cnt
        wait mvarDat

    parser = do
      string _CONTENT_LENGTH
      len <- manyTill digit (string _TWO_CRLF)
      return . read $ len

    unexpectedEOF = do
      L.criticalM _LOG_NAME "unexpected EOF from stdin."
      return ()



-- |
--
sendResponse :: BSL.ByteString -> IO ()
sendResponse str = do
  BSL.hPut stdout $ str2lbs $ _CONTENT_LENGTH ++ (show (BSL.length str))
  BSL.hPut stdout $ str2lbs _TWO_CRLF
  BSL.hPut stdout str
  hFlush stdout

