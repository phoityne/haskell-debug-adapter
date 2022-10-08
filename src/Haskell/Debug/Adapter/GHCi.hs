{-# LANGUAGE LambdaCase #-}

module Haskell.Debug.Adapter.GHCi where

import Control.Monad.IO.Class
import Control.Lens
import qualified Data.ByteString as B
import Control.Concurrent.MVar
import Control.Monad.State.Lazy
import Control.Monad.Except
import GHC.IO.Encoding
import Distribution.System
import qualified System.Process as S
import qualified System.IO as S
import qualified System.Environment as S
import qualified Data.Map as M
import qualified Data.List as L

import Haskell.Debug.Adapter.Type
import qualified Haskell.Debug.Adapter.Utility as U
import Haskell.Debug.Adapter.Constant

---------------------------------------------------------------------------------
-- |
--  run ghci.
--
startGHCi :: String
          -> [String]
          -> FilePath
          -> M.Map String String
          -> AppContext ()
startGHCi cmd opts cwd envs =
  U.liftIOE (startGHCiIO cmd opts cwd envs) >>= liftEither >>= updateGHCi

  where
    updateGHCi proc = do
      mvar <- view ghciProcAppStores <$> get
      --_ <- liftIO $ takeMVar mvar
      liftIO $ putMVar mvar proc


-- |
--
startGHCiIO :: String
            -> [String]
            -> FilePath
            -> M.Map String String
            -> IO (Either ErrMsg GHCiProc)
startGHCiIO cmd opts cwd envs = do

  (fromPhoityneHandle, toGHCiHandle) <- S.createPipe
  (fromGHCiHandle, toPhoityneHandle) <- S.createPipe

  osEnc <- getReadHandleEncoding

  let bufMode = S.NoBuffering
  --let bufMode = S.BlockBuffering $ Just 1024

  S.hSetBuffering toPhoityneHandle bufMode
  S.hSetEncoding toPhoityneHandle osEnc
  S.hSetNewlineMode toPhoityneHandle $ S.NewlineMode S.CRLF S.LF
  --S.hSetBinaryMode toPhoityneHandle True

  S.hSetBuffering fromPhoityneHandle bufMode
  S.hSetEncoding fromPhoityneHandle  S.utf8
  S.hSetNewlineMode fromPhoityneHandle $ S.NewlineMode S.LF S.LF
  --S.hSetBinaryMode fromPhoityneHandle True

  S.hSetBuffering toGHCiHandle bufMode
  S.hSetEncoding toGHCiHandle S.utf8
  S.hSetNewlineMode toGHCiHandle $ S.NewlineMode S.LF S.LF
  --S.hSetBinaryMode toGHCiHandle True

  S.hSetBuffering fromGHCiHandle bufMode
  S.hSetEncoding fromGHCiHandle osEnc
  S.hSetNewlineMode fromGHCiHandle $ S.NewlineMode S.CRLF S.LF
  --S.hSetBinaryMode fromGHCiHandle True

  runEnvs <- getRunEnv

  ghciGHCi <- S.runProcess cmd opts (Just cwd) runEnvs (Just fromPhoityneHandle) (Just toPhoityneHandle) (Just toPhoityneHandle)

  return . Right $ GHCiProc toGHCiHandle fromGHCiHandle fromGHCiHandle ghciGHCi

  where
    -- |
    --
    getReadHandleEncoding :: IO TextEncoding
    getReadHandleEncoding = if
      | Windows == buildOS -> mkTextEncoding "CP932//TRANSLIT"
      | otherwise          -> mkTextEncoding "UTF-8//TRANSLIT"

    -- |
    --
    getRunEnv
      | null envs = return Nothing
      | otherwise = do
          curEnvs <- S.getEnvironment
          return $ Just $ M.toList envs ++ curEnvs


-- |
--  write to ghci.
--
command :: String -> AppContext ()
command cmd = do
  mver <- view ghciProcAppStores <$> get
  proc <- U.liftIOE $ readMVar mver
  let hdl = proc^.wHdLGHCiProc

  U.liftIOE $ S.hPutStrLn hdl cmd
  pout cmd

  where
    pout s
      | L.isPrefixOf ":dap-" s = U.sendStdoutEventLF $ (takeWhile ((/=) ' ') s) ++ " ..."
      | otherwise = U.sendStdoutEventLF s


-- |
--
expectInitPmpt :: String -> AppContext [String]
expectInitPmpt pmpt = do
  mvar <- view ghciProcAppStores <$> get
  proc <- U.liftIOE $ readMVar mvar
  let hdl = proc^.rHdlGHCiProc

  go pmpt hdl "" >>= \case
    Right xs -> do
      let strs = map U.rstrip $ lines xs
      pout strs
      return strs
    Left xs -> do
      let strs = map U.rstrip $ lines xs
      pout strs
      throwError "[CRITICAL] can not get the initial ghci prompt."

  where
    go :: String -> S.Handle -> String -> AppContext (Either String String)
    go key hdl acc = catchError
      (U.readChar hdl >>= byPmpt key hdl acc)
      (errHdl acc)

    errHdl :: String -> String -> AppContext (Either String String)
    errHdl acc e = return $ Left $ unlines [acc, "", e, ""]

    byPmpt :: String -> S.Handle -> String -> String -> AppContext (Either String String)
    byPmpt key hdl acc b = do
      let newAcc = acc ++ b
      U.debugEV _LOG_GHCI_STDOUT newAcc
      if L.isSuffixOf key newAcc
        then return $ Right newAcc
        else go key hdl newAcc

    pout [] = return ()
    pout (x:[]) = U.sendStdoutEvent x
    pout (x:xs) = U.sendStdoutEventLF x >> pout xs

-- |
--
expectPmpt :: AppContext [String]
expectPmpt = do
  pmpt <- view ghciPmptAppStores <$> get
  mvar <- view ghciProcAppStores <$> get
  proc <- U.liftIOE $ readMVar mvar
  let hdl = proc^.rHdlGHCiProc
      plen = length pmpt

  go plen hdl []

  where
    go plen hdl acc = U.liftIOE (S.hIsEOF hdl) >>= \case
      True  -> return acc
      False -> U.readLine hdl >>= byLine plen hdl acc

    byLine plen hdl acc line
      | L.isSuffixOf _DAP_CMD_END2 line = goEnd plen hdl acc
      | otherwise = cont plen hdl acc line

    cont plen hdl acc l = do
      when (not (U.startswith _DAP_HEADER l)) $ U.sendStdoutEventLF l
      go plen hdl $ acc ++ [l]

    goEnd plen hdl acc = do
      b <- U.liftIOE $ B.hGet hdl plen
      let l = U.bs2str b
      U.sendStdoutEvent l
      return $ acc ++ [l]

