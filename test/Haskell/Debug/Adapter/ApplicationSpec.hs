{-# LANGUAGE OverloadedStrings #-}

module Haskell.Debug.Adapter.ApplicationSpec where

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

import qualified Spec.Utility as U

import qualified GHCi.DAP.IFData as DAP
import Haskell.Debug.Adapter.Application
import Haskell.Debug.Adapter.Type
import Haskell.Debug.Adapter.Utility
import Haskell.Debug.Adapter.Constant
import qualified Haskell.Debug.Adapter.Thread as TD
import qualified Haskell.Debug.Adapter.Event  as EV

-- |
--
spec :: Spec
spec = do
  runIO $ putStrLn "Start Haskell.Debug.Adapter.ApplicationSpec"
  beforeAll_ beforeAll'
    . afterAll_ afterAll'
    . before_ before'
    . after_  after'
    $ spec'

  where
  
    beforeAll' :: IO ()
    beforeAll' = do
      U.setUpLogger L.DEBUG
      return ()

    afterAll' ::IO ()
    afterAll' = do
      U.tearDownLogger
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
            dat <- defaultAppStores

            TD.start dat [run dat, client dat] >>= wait

            ress <- readMVar $ dat^.resStoreAppStores
            length ress `shouldBe` 2

      describe "appMain" $ do
        context "when init state, initialize request." $ do
          it "should send initialize response." $ do
            def <- defaultAppStores
            let dat = def {_appStateWAppStores = WrapAppState InitState}
                req = WrapRequest $ InitializeRequest DAP.defaultInitializeRequest

            ret <- runApp dat $ appMain req

            ress <- readMVar $ dat^.resStoreAppStores
            length ress `shouldBe` 1

        context "when init state, launch request." $ do
          it "should send launch response." $ do
            def <- defaultAppStores
            let dat = def {_appStateWAppStores = WrapAppState InitState}
                req = WrapRequest $ LaunchRequest DAP.defaultLaunchRequest

            ret <- runApp dat $ appMain req

            ress <- readMVar $ dat^.resStoreAppStores
            length ress `shouldBe` 1


    -- |
    --
    client dat = do
      let req = WrapRequest $ InitializeRequest DAP.defaultInitializeRequest

      request dat req

      threadDelay (2 * 1000 * 1000)

      request dat req

      threadDelay (2 * 1000 * 1000)

      EV.addEvent dat StopEvent
      return ()

-- |
--
request :: AppStores -> WrapRequest -> IO ()
request dat req = do
  let reqsMVar = dat^.reqStoreAppStores
  reqs <- takeMVar reqsMVar
  putMVar reqsMVar (reqs++[req])
