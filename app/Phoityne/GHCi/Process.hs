{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable  #-}

module Phoityne.GHCi.Process (
    ErrorData
  , GHCiProcess (..)
  , Lock(..)
  , _BASE_GHCI_VERSION
  , runProcess
  , exitProcess
  , writeLine
  , readTillPrompt
  , readTillEOF
  , readCharWhile
  , readCharWhileIO
  , readLineWhile
  , readLineWhileIO
  ) where

import Control.Concurrent
import GHC.IO.Encoding
import Distribution.System
import qualified System.Process as S
import qualified System.IO as S
import qualified System.Exit as S
import qualified System.Environment as S
import qualified Control.Exception.Safe as E
import qualified Data.String.Utils as U
import qualified Data.Map as M
import qualified Data.Version as V

-- |
--  command error message.
--
type ErrorData = String

-- |
--  command error message.
--
_BASE_GHCI_VERSION :: V.Version
_BASE_GHCI_VERSION =  V.Version [8, 0, 0] []


-- |
--  command error message.
--
data Lock = Lock

-- |
--   GHCi process data.
--
data GHCiProcess = GHCiProcess
  {
    inGHCiProcess      :: S.Handle
  , outGHCiProcess     :: S.Handle
  , errGHCiProcess     :: S.Handle
  , procGHCiProcess    :: S.ProcessHandle
  , promptGHCiProcess  :: String
  , versionGHCiProcess :: V.Version
  , lockGHCiProcess    :: MVar Lock
  }

-- |
--  run ghci.
--
runProcess :: String
           -> [String]
           -> FilePath
           -> String
           -> M.Map String String
           -> IO (Either ErrorData GHCiProcess)
runProcess cmd opts cwd pmt envs = flip E.catches handlers $ do

  (fromPhoityneHandle, toGHCiHandle) <- S.createPipe
  (fromGHCiHandle, toPhoityneHandle) <- S.createPipe

  osEnc <- getReadHandleEncoding

  S.hSetBuffering toPhoityneHandle S.NoBuffering
  S.hSetEncoding toPhoityneHandle osEnc
  S.hSetNewlineMode toPhoityneHandle $ S.NewlineMode S.CRLF S.LF

  S.hSetBuffering fromPhoityneHandle S.NoBuffering
  S.hSetEncoding fromPhoityneHandle  S.utf8
  S.hSetNewlineMode fromPhoityneHandle $ S.NewlineMode S.LF S.LF

  S.hSetBuffering toGHCiHandle S.NoBuffering
  S.hSetEncoding toGHCiHandle S.utf8
  S.hSetNewlineMode toGHCiHandle $ S.NewlineMode S.LF S.LF

  S.hSetBuffering fromGHCiHandle S.NoBuffering
  S.hSetEncoding fromGHCiHandle osEnc
  S.hSetNewlineMode fromGHCiHandle $ S.NewlineMode S.CRLF S.LF

  runEnvs <- getRunEnv

  ghciProc <- S.runProcess cmd opts (Just cwd) runEnvs (Just fromPhoityneHandle) (Just toPhoityneHandle) (Just toPhoityneHandle)

  mvarLock <- newMVar Lock

  return . Right $ GHCiProcess toGHCiHandle fromGHCiHandle fromGHCiHandle ghciProc pmt _BASE_GHCI_VERSION mvarLock

  where
    handlers = [ E.Handler someExcept ]
    someExcept (e :: E.SomeException) = return . Left . show $ e

    -- |
    --  
    -- 
    getReadHandleEncoding :: IO TextEncoding
    getReadHandleEncoding = if
      | Windows == buildOS -> mkTextEncoding "CP932//TRANSLIT"
      | otherwise -> mkTextEncoding "UTF-8//TRANSLIT"

    -- |
    --  
    -- 
    getRunEnv
      | null envs = return Nothing
      | otherwise = do
          curEnvs <- S.getEnvironment
          return $ Just $ M.toList envs ++ curEnvs 

-- |
--   exit ghci.
--
exitProcess :: GHCiProcess -> IO (Either ErrorData S.ExitCode)
exitProcess (GHCiProcess _ _ _ proc _ _ _) = flip E.catches handlers $ do
  code <- S.waitForProcess proc
  return . Right $ code
  where
    handlers = [ E.Handler someExcept ]
    someExcept (e :: E.SomeException) = return . Left . show $ e

-- |
--  write to ghci.
--
writeLine :: GHCiProcess -> String -> IO (Either ErrorData ())
writeLine (GHCiProcess ghciIn _ _ _ _ _ _) writeData = flip E.catches handlers $ S.hIsOpen ghciIn >>= \case
  True  -> do
    S.hPutStrLn ghciIn writeData
    return $ Right ()
  False -> return $ Left "handle not open."
  where
    handlers = [ E.Handler someExcept ]
    someExcept (e :: E.SomeException) = return . Left . show $ e

-- |
--   read char till prompt.
--
readTillPrompt :: GHCiProcess -> IO (Either ErrorData String)
readTillPrompt proc@(GHCiProcess _ _ _ _ pmt _ _) = readCharWhile proc (not . U.endswith pmt)

-- |
--   read char till EOF.
--
readTillEOF :: GHCiProcess -> IO (Either ErrorData String)
readTillEOF proc = readCharWhile proc (const True)


-- |
--   read char from ghci.
--
readCharWhile :: GHCiProcess -> (String -> Bool) -> IO (Either ErrorData String)
readCharWhile (GHCiProcess _ ghciOut _ _ _ _ _) condProc = flip E.catches handlers $ S.hIsOpen ghciOut >>= \case
  True  -> go []
  False -> return . Left $ "handle not open."
  where
    go acc = S.hIsEOF ghciOut >>= \case
      True  -> return . Right $ acc
      False -> do
        c <- S.hGetChar ghciOut
        let acc' = acc ++ [c]
        if condProc acc' then go acc'
          else return . Right $ acc'

    handlers = [ E.Handler someExcept ]
    someExcept (e :: E.SomeException) = return . Left . show $ e

-- |
--   read char from ghci.
--
readCharWhileIO :: GHCiProcess -> (String -> IO Bool) -> IO (Either ErrorData String)
readCharWhileIO (GHCiProcess _ ghciOut _ _ _ _ _) condProc = flip E.catches handlers $ S.hIsOpen ghciOut >>= \case
  True  -> go []
  False -> return . Left $ "handle not open."
  where
    go acc = S.hIsEOF ghciOut >>= \case
      True  -> return . Right $ acc
      False -> do
        c <- S.hGetChar ghciOut
        let acc' = acc ++ [c]
        condProc acc' >>= \case 
          True  -> go acc'
          False -> return . Right $ acc'

    handlers = [ E.Handler someExcept ]
    someExcept (e :: E.SomeException) = return . Left . show $ e

-- |
--  read line from ghci.
--
readLineWhile :: GHCiProcess -> ([String] -> Bool) -> IO (Either ErrorData [String])
readLineWhile (GHCiProcess _ ghciOut _ _ _ _ _) condProc = flip E.catches handlers $ S.hIsOpen ghciOut >>= \case
  True  -> go []
  False -> return . Left $ "handle not open."
  where
    go acc = S.hIsEOF ghciOut >>= \case
      True -> return . Right $ acc
      False -> do
        l <- S.hGetLine ghciOut
        let acc' = acc ++ [l]
        if condProc acc' then go acc'
          else return . Right $ acc'

    handlers = [ E.Handler someExcept ]
    someExcept (e :: E.SomeException) = return . Left . show $ e


-- |
--  read line from ghci.
--
readLineWhileIO :: GHCiProcess -> (String -> [String] -> IO Bool) -> IO (Either ErrorData [String])
readLineWhileIO (GHCiProcess _ ghciOut _ _ _ _ _) condProc = flip E.catches handlers $ S.hIsOpen ghciOut >>= \case
  True  -> go []
  False -> return . Left $ "handle not open."
  where
    go acc = S.hIsEOF ghciOut >>= \case
      True -> return . Right $ acc
      False -> do
        l <- S.hGetLine ghciOut
        let acc' = acc ++ [l]
        condProc l acc' >>= \case
          True  -> go acc'
          False -> return . Right $ acc'

    handlers = [ E.Handler someExcept ]
    someExcept (e :: E.SomeException) = return . Left . show $ e
