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
import qualified Control.Exception.Safe as E
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
  liftIO (startGHCiIO cmd opts cwd envs) >>= liftEither >>= updateGHCi

  where
    updateGHCi proc = do
      mvar <- view ghciGHCiAppStores <$> get
      --_ <- liftIO $ takeMVar mvar
      liftIO $ putMVar mvar proc


-- |
--
startGHCiIO :: String
            -> [String]
            -> FilePath
            -> M.Map String String
            -> IO (Either ErrMsg GHCiGHCi)
startGHCiIO cmd opts cwd envs = flip E.catches handlers $ do

  (fromPhoityneHandle, toGHCiHandle) <- S.createPipe
  (fromGHCiHandle, toPhoityneHandle) <- S.createPipe

  osEnc <- getReadHandleEncoding

  let bufMode = S.NoBuffering
  --let bufMode = S.BlockBuffering $ Just 1024

  S.hSetBuffering toPhoityneHandle bufMode
  S.hSetEncoding toPhoityneHandle osEnc
  S.hSetNewlineMode toPhoityneHandle $ S.NewlineMode S.CRLF S.LF
  S.hSetBinaryMode toPhoityneHandle True

  S.hSetBuffering fromPhoityneHandle bufMode
  S.hSetEncoding fromPhoityneHandle  S.utf8
  S.hSetNewlineMode fromPhoityneHandle $ S.NewlineMode S.LF S.LF
  S.hSetBinaryMode fromPhoityneHandle True

  S.hSetBuffering toGHCiHandle bufMode
  S.hSetEncoding toGHCiHandle S.utf8
  S.hSetNewlineMode toGHCiHandle $ S.NewlineMode S.LF S.LF
  S.hSetBinaryMode toGHCiHandle True

  S.hSetBuffering fromGHCiHandle bufMode
  S.hSetEncoding fromGHCiHandle osEnc
  S.hSetNewlineMode fromGHCiHandle $ S.NewlineMode S.CRLF S.LF
  S.hSetBinaryMode fromGHCiHandle True

  runEnvs <- getRunEnv

  ghciGHCi <- S.runProcess cmd opts (Just cwd) runEnvs (Just fromPhoityneHandle) (Just toPhoityneHandle) (Just toPhoityneHandle)

  return . Right $ GHCiGHCi toGHCiHandle fromGHCiHandle fromGHCiHandle ghciGHCi

  where
    handlers = [ E.Handler someExcept ]
    someExcept (e :: E.SomeException) = return . Left . show $ e

    -- |
    -- 
    getReadHandleEncoding :: IO TextEncoding
    getReadHandleEncoding = if
      | Windows == buildOS -> mkTextEncoding "CP932//TRANSLIT"
      | otherwise -> mkTextEncoding "UTF-8//TRANSLIT"

    -- |
    -- 
    getRunEnv
      | null envs = return Nothing
      | otherwise = do
          curEnvs <- S.getEnvironment
          return $ Just $ M.toList envs ++ curEnvs 


-- |
--
type ExpectCallBack = Bool -> [String] -> [String] -> AppContext ()

-- |
--
expect :: ExpectCallBack -> AppContext ()
expect func = do
  pmpt <- view ghciPmptAppStores <$> get
  mvar <- view ghciGHCiAppStores <$> get
  proc <- liftIO $ readMVar mvar
  let hdl = proc^.rHdlGHCiGHCi
      plen = length pmpt

  go plen hdl []
  
  where
    go plen hdl acc = do
      b <- liftIO $ B.hGetLine hdl
      let newL = U.bs2str b
      if L.isSuffixOf _DAP_CMD_END2 newL
        then goEnd plen hdl acc
        else cont plen hdl acc newL

    cont plen hdl acc newL = do
      let newAcc = acc ++ [newL]
      func False newAcc [newL]
      go plen hdl newAcc

    goEnd plen hdl acc = do
      b <- liftIO $ B.hGet hdl plen
      let l = U.bs2str b
          newAcc = acc ++ [l]

      func True newAcc [l]


-- |
--
expectS :: String -> ExpectCallBack -> AppContext ()
expectS key func = do
  mvar <- view ghciGHCiAppStores <$> get
  proc <- liftIO $ readMVar mvar
  let hdl = proc^.rHdlGHCiGHCi

  bs <- liftIO $ go (U.str2bs key) hdl B.empty
  let strs = map rstrip $ lines $ U.bs2str bs

  func True strs strs

  where
    go kb hdl acc = do
      b <- B.hGet hdl 1
      let newAcc = B.append acc b
      if B.isSuffixOf kb newAcc
        then return newAcc
        else go kb hdl newAcc

    rstrip :: [Char] -> [Char]
    rstrip [] = []
    rstrip xs
      | last xs == '\r' = init xs
      | otherwise = xs

-- |
--  write to ghci.
--
command :: String -> AppContext ()
command cmd = do
  mver <- view ghciGHCiAppStores <$> get
  proc <- liftIO $ readMVar mver
  let hdl = proc^.wHdLGHCiGHCi

  liftIO (goIO hdl cmd) >>= liftEither

  where
    goIO hdl cmd = flip E.catchAny errHdl $ do
      Right <$> S.hPutStrLn hdl cmd

    errHdl e = return $ Left $ show e

-- |
--  write to ghci.
--
stdoutCallBk :: Bool -> [String] -> [String] -> AppContext ()
stdoutCallBk _ _ ([]) = return ()
stdoutCallBk True  _ (x:[]) = U.sendStdoutEvent x
stdoutCallBk False _ (x:[]) = U.sendStdoutEventLF x
stdoutCallBk True  _ xs = do
  mapM_ U.sendStdoutEventLF $ init xs
  U.sendStdoutEvent $ last xs
stdoutCallBk False _ xs = mapM_ U.sendStdoutEventLF xs


-- |
--
cmdAndOut :: String -> AppContext ()
cmdAndOut cmd = U.sendStdoutEventLF cmd >>  command cmd


-- |
--  write to ghci.
--
funcCallBk :: (Bool -> String -> AppContext ()) -> Bool -> [String] -> [String] -> AppContext ()
funcCallBk _ _ _ ([]) = return ()
funcCallBk f b _ (x:[]) = f b x
funcCallBk f True  _ xs = do
  mapM_ (f False) $ init xs
  f True $ last xs
funcCallBk f False _ xs = mapM_ (f False) xs
