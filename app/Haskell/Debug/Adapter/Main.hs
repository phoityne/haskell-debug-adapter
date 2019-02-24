module Haskell.Debug.Adapter.Main (run) where

import qualified System.IO as S
import qualified Control.Exception.Safe as E

import qualified Haskell.Debug.Adapter.Control as CTRL



-- |
--  Application Main
-- 
run :: CTRL.ArgData -> IO Int
run args = flip E.catchAny ehdl $ do

  -- run logic main
  flip E.finally finalize $ CTRL.run args S.stdin S.stdout

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

