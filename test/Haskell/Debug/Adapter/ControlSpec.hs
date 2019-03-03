{-# LANGUAGE OverloadedStrings #-}

module Haskell.Debug.Adapter.ControlSpec where

import Test.Hspec
import Data.Aeson
import Data.Default
import Control.Concurrent (threadDelay)
import qualified System.IO as S
import Control.Concurrent.Async

import Spec.Utility

import qualified Haskell.DAP as DAP
import Haskell.Debug.Adapter.Control

-- |
--
spec :: Spec
spec = do
  runIO $ putStrLn "Start Haskell.Debug.Adapter.ControlSpec"
  beforeAll_ beforeAll'
    . afterAll_ afterAll'
    . before_ before'
    . after_  after'
    $ spec'

  where
  
    beforeAll' :: IO ()
    beforeAll' = do
      return ()

    afterAll' ::IO ()
    afterAll' = do
      return ()

    before' :: IO ()
    before' = return ()

    after' :: IO ()
    after' = return ()
    

    spec' :: Spec
    spec' = do
      describe "run" $ do
        context "when default args" $ 
          xit "should be 0" $ do
            let arg  = def
            (fromClient, toClient) <- createPipe
            (fromServer, toServer) <- createPipe

            (res, _) <- runConcurrently $ (,)
              <$> Concurrently (run arg fromClient toClient)
              <*> Concurrently (client toServer)

            res `shouldBe` 0

            S.hClose fromClient
            S.hClose toClient
            S.hClose fromServer
            S.hClose toServer

    client wHdl = do
      let req = DAP.defaultInitializeRequest
          jsonBS = encode req

      request wHdl jsonBS

      threadDelay _1_SEC

      return ()
