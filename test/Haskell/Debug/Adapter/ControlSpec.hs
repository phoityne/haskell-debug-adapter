{-# LANGUAGE OverloadedStrings #-}

module Haskell.Debug.Adapter.ControlSpec where

import Test.Hspec
import Data.Default
import qualified System.Log.Logger as L

import Spec.Utility

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
        context "when default args" $ 
          xit "should be 1" $ do
            let arg  = def
                conf = def
            res <- run arg conf
            res `shouldBe` 1

        context "when args" $ 
          it "should throw exception" $ do
            let arg  = def
                conf = def
            run arg conf `shouldThrow` anyException
