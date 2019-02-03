{-# LANGUAGE OverloadedStrings #-}

module Haskell.Debug.Adapter.ResponseSpec where

import Test.Hspec
import Data.Aeson
import Control.Lens
import Control.Concurrent.MVar
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import qualified System.IO as S
import qualified System.Process as S
import Control.Concurrent.Async
import qualified Data.ByteString.Lazy as B
import Control.Exception.Safe
import qualified System.Log.Logger as L

import Spec.Utility

import qualified GHCi.DAP.IFData as DAP
import qualified Haskell.Debug.Adapter.Application as A
import Haskell.Debug.Adapter.Response
import Haskell.Debug.Adapter.Type
import Haskell.Debug.Adapter.Utility
import Haskell.Debug.Adapter.Constant
import qualified Haskell.Debug.Adapter.Thread as TD
import qualified Haskell.Debug.Adapter.Event  as EV

-- |
--
spec :: Spec
spec = do
  runIO $ putStrLn "Start Haskell.Debug.Adapter.ResponseSpec"
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
    spec' = do
      xdescribe "run" $ do
        context "when default args" $ do
          it "should send 2 response." $ do
            (rHdl, wHdl) <- createPipe
            bufMVar <- newMVar B.empty
            appDefDat <- A.defaultAppStores
            let dat = appDefDat {
                      _inHandleAppStores  = S.stdin
                    , _outHandleAppStores = wHdl
                    }
                    

            TD.start dat [run dat, client dat, read2buf rHdl bufMVar] >>= wait

            ress <- readMVar $ dat^.resStoreAppStores
            length ress `shouldBe` 0
            readMVar bufMVar >>= print . lbs2str

            S.hClose wHdl
            S.hClose rHdl


    client dat = do
      let res = InitializeResponse DAP.defaultInitializeResponse

      response dat res

      threadDelay (2 * 1000 * 1000)

      response dat res

      threadDelay (2 * 1000 * 1000)

      EV.addEvent dat StopEvent
      return ()

-- |
--
response :: AppStores -> Response -> IO ()
response dat res = do
  let ressMVar = dat^.resStoreAppStores
  ress <- takeMVar ressMVar
  putMVar ressMVar (ress++[res])

