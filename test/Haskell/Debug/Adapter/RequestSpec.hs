{-# LANGUAGE OverloadedStrings #-}

module Haskell.Debug.Adapter.RequestSpec where

import Test.Hspec
import Data.Aeson
import Control.Lens
import Control.Concurrent.MVar
import Control.Concurrent (threadDelay)
import qualified System.IO as S
import qualified System.Process as S
import Control.Concurrent.Async
import qualified Data.ByteString as B
import qualified System.Log.Logger as L

import Spec.Utility

import qualified GHCi.DAP.IFData as DAP
import qualified Haskell.Debug.Adapter.Application as A
import Haskell.Debug.Adapter.Request
import Haskell.Debug.Adapter.Type
import Haskell.Debug.Adapter.Utility
import Haskell.Debug.Adapter.Constant

-- |
--
spec :: Spec
spec = do
  runIO $ putStrLn "Start Haskell.Debug.Adapter.RequestSpec"
  beforeAll_ beforeAll'
    . afterAll_ afterAll'
    . before_ before'
    . after_  after'
    $ spec'

  where
  
    beforeAll' :: IO ()
    beforeAll' = do
      setUpLogger L.DEBUG
      return ()

    afterAll' ::IO ()
    afterAll' = do
      tearDownLogger
      return ()

    before' :: IO ()
    before' = return ()

    after' :: IO ()
    after' = return ()

    spec' :: Spec
    spec' =  do
      xdescribe "run" $ do
        context "when default args" $ 
          it "should be stored 2 request." $ do
            (rHdl, wHdl) <- createPipe
            appDefDat <- A.defaultAppStores
            let dat = appDefDat {
                      _inHandleAppStores    = rHdl
                    , _outHandleAppStores   = S.stdout
                    }
                    

            (res, _) <- runConcurrently $ (,)
              <$> Concurrently (run dat)
              <*> Concurrently (client wHdl)

            reqs <- readMVar $ dat^.reqStoreAppStores
            length reqs `shouldBe` 2

            S.hClose rHdl
            S.hClose wHdl
        
    client wHdl = do
      let req = DAP.defaultInitializeRequest
          jsonStr = encode req
          jsonBS = str2bs $ lbs2str jsonStr

      request wHdl jsonBS

      threadDelay (2 * 1000 * 1000)

      request wHdl jsonBS

      threadDelay (2 * 1000 * 1000)

      S.hClose wHdl
      return ()


-- |
--
request :: S.Handle -> B.ByteString -> IO ()
request hdl jsonBS = do
  let jsonLen = str2bs $ show $ B.length jsonBS
  B.hPut hdl $ str2bs _CONTENT_LENGTH
  B.hPut hdl jsonLen
  B.hPut hdl $ str2bs _TWO_CRLF
  B.hPut hdl jsonBS

