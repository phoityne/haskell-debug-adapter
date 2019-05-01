{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Haskell.Debug.Adapter.State.DebugRun.Continue where

import Control.Monad.IO.Class
import qualified System.Log.Logger as L
import qualified Text.Read as R
import qualified Data.List as L
import Control.Monad.Except

import qualified Haskell.DAP as DAP
import Haskell.Debug.Adapter.Type
import Haskell.Debug.Adapter.Constant
import qualified Haskell.Debug.Adapter.Utility as U
import qualified Haskell.Debug.Adapter.GHCi as P

-- |
--  Any errors should be sent back as False result Response
--
instance StateActivityIF DebugRunStateData DAP.ContinueRequest where
  action2 _ (ContinueRequest req) = do
    liftIO $ L.debugM _LOG_APP $ "DebugRunState ContinueRequest called. " ++ show req
    app req

-- |
--
app :: DAP.ContinueRequest -> AppContext (Maybe StateTransit)
app req = flip catchError errHdl $ do

  let args = DAP.argumentsContinueRequest req
      dap = ":dap-continue "
      cmd = dap ++ U.showDAP args
      dbg = dap ++ show args

  P.cmdAndOut cmd
  U.debugEV _LOG_APP dbg
  P.expectH $ P.funcCallBk lineCallBk

  resSeq <- U.getIncreasedResponseSequence
  let res = DAP.defaultContinueResponse {
            DAP.seqContinueResponse = resSeq
          , DAP.request_seqContinueResponse = DAP.seqContinueRequest req
          , DAP.successContinueResponse = True
          }

  U.addResponse $ ContinueResponse res

  return Nothing

  where
    lineCallBk :: Bool -> String -> AppContext ()
    lineCallBk True  s = U.sendStdoutEvent s
    lineCallBk False s
      | L.isPrefixOf _DAP_HEADER s = do
        U.debugEV _LOG_APP s
        dapHdl $ drop (length _DAP_HEADER) s
      | otherwise = U.sendStdoutEventLF s

    -- |
    --
    dapHdl :: String -> AppContext ()
    dapHdl str = case R.readEither str of
      Left err -> errHdl err >> return ()
      Right (Left err) -> errHdl err >> return ()
      Right (Right body) -> U.handleStoppedEventBody body


    -- |
    --
    errHdl :: String -> AppContext (Maybe StateTransit)
    errHdl msg = do
      liftIO $ L.errorM _LOG_APP msg
      resSeq <- U.getIncreasedResponseSequence
      let res = DAP.defaultContinueResponse {
                DAP.seqContinueResponse = resSeq
              , DAP.request_seqContinueResponse = DAP.seqContinueRequest req
              , DAP.successContinueResponse = False
              , DAP.messageContinueResponse = msg
              }

      U.addResponse $ ContinueResponse res
      return Nothing

