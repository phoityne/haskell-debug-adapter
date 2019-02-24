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
import qualified Data.String.Utils as U
--import qualified System.Log.Logger as L

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
startGHCiIO cmd opts cwd envs = flip E.catches handlers $ do

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
--   expect prompt or eof
-- 
expectEOF :: ExpectCallBack -> AppContext [String]
expectEOF func = expectH' True func

-- |
--   expect prompt. eof throwError.
--
expectH :: ExpectCallBack -> AppContext [String]
expectH func = expectH' False func

-- |
--
expectH' :: Bool -> ExpectCallBack -> AppContext [String]
expectH' tilEOF func = do
  pmpt <- view ghciPmptAppStores <$> get
  mvar <- view ghciProcAppStores <$> get
  proc <- liftIO $ readMVar mvar
  let hdl = proc^.rHdlGHCiProc
      plen = length pmpt

  go tilEOF plen hdl []
  
  where
    go False plen hdl acc = U.readLine hdl >>= go' plen hdl acc
    go True plen hdl acc = liftIO (S.hIsEOF hdl) >>= \case
      False -> U.readLine hdl >>= go' plen hdl acc
      True  -> return acc

    go' plen hdl acc b = do
      let newL = U.rstrip b
      if L.isSuffixOf _DAP_CMD_END2 newL
        then goEnd plen hdl acc
        else cont plen hdl acc newL

    cont plen hdl acc newL = do
      let newAcc = acc ++ [newL]
      func False newAcc [newL]
      go tilEOF plen hdl newAcc

    goEnd plen hdl acc = do
      b <- liftIO $ B.hGet hdl plen
      let l = U.bs2str b
          newAcc = acc ++ [l]

      func True newAcc [l]
      return newAcc


-- |
--
expect :: String -> ExpectCallBack -> AppContext ()
expect key func = do
  mvar <- view ghciProcAppStores <$> get
  proc <- liftIO $ readMVar mvar
  let hdl = proc^.rHdlGHCiProc

  xs <- go key hdl ""
  let strs = map U.rstrip $ lines xs

  func True strs strs

  where
    go kb hdl acc = U.readChar hdl
      >>= go' kb hdl acc

    go' kb hdl acc b = do
      let newAcc = acc ++ b
      if L.isSuffixOf kb newAcc
        then return newAcc
        else go kb hdl newAcc


-- |
--  write to ghci.
--
command :: String -> AppContext ()
command cmd = do
  mver <- view ghciProcAppStores <$> get
  proc <- liftIO $ readMVar mver
  let hdl = proc^.wHdLGHCiProc

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
cmdAndOut cmd = do
  pout cmd
  command cmd
  where
    pout s
      | L.isPrefixOf ":dap-" s = U.sendStdoutEventLF $ (takeWhile ((/=) ' ') s) ++ " ..."
      | otherwise = U.sendStdoutEventLF s
      

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
