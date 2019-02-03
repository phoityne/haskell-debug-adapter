module Haskell.Debug.Adapter.Main (run) where

import Haskell.Debug.Adapter.Type
import Haskell.Debug.Adapter.Config
import qualified Haskell.Debug.Adapter.Control as CTRL

import qualified Control.Exception.Safe as E


-- |
--  Application Main
-- 
run :: ArgData -> IO Int
run args = flip E.catchAny ehdl $ do

  -- load config setting
  conf <- getConfigData args

  -- run logic main
  flip E.finally finalize $ CTRL.run args conf

  where
    finalize = return () 
    ehdl e   = putStrLn (criticalMsg e) >> return 1

    criticalMsg e = unlines [
        ""
      , "[CRITICAL] unexpected error. exit 1."
      , "--------------------------------------------------------------------------------"
      , show e
      , "--------------------------------------------------------------------------------"
      ]

