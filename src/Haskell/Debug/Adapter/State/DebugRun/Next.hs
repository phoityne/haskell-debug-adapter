{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Haskell.Debug.Adapter.State.DebugRun.Next where

import Control.Monad.IO.Class
import qualified System.Log.Logger as L
import qualified Text.Read as R
import qualified Data.List as L

import qualified GHCi.DAP as DAP
import Haskell.Debug.Adapter.Type
import Haskell.Debug.Adapter.Constant
import qualified Haskell.Debug.Adapter.Utility as U
import qualified Haskell.Debug.Adapter.GHCi as P

instance StateRequestIF DebugRunState DAP.NextRequest where
  --action :: (StateRequest s r) -> AppContext ()
  action (DebugRun_Next req) = do
    liftIO $ L.debugM _LOG_APP $ "DebugRunState NextRequest called. " ++ show req
    app req

-- |
--
app :: DAP.NextRequest -> AppContext (Maybe StateTransit)
app req = do
  
  let args = DAP.argumentsNextRequest req
      cmd = ":dap-next " ++ U.showDAP args

  P.cmdAndOut cmd
  P.expectH $ P.funcCallBk lineCallBk

  resSeq <- U.getIncreasedResponseSequence
  let res = DAP.defaultNextResponse {
            DAP.seqNextResponse = resSeq
          , DAP.request_seqNextResponse = DAP.seqNextRequest req
          , DAP.successNextResponse = True
          }

  U.addResponse $ NextResponse res

  return Nothing
  
  where
    lineCallBk :: Bool -> String -> AppContext ()
    lineCallBk True  s = U.sendStdoutEvent s
    lineCallBk False s
      | L.isPrefixOf _DAP_HEADER s = dapHdl $ drop (length _DAP_HEADER) s
      | otherwise = U.sendStdoutEventLF s

    -- |
    --
    dapHdl :: String -> AppContext ()
    dapHdl str = case R.readEither str of
      Left err -> errHdl str err
      Right (Left err) -> errHdl str err
      Right (Right body) -> U.handleStoppeEventBody body
        

    -- |
    --
    errHdl :: String -> String -> AppContext()
    errHdl str err = do
      let msg = "next request failed. " ++ err ++ " : " ++ str
      liftIO $ L.errorM _LOG_APP msg
      resSeq <- U.getIncreasedResponseSequence
      let res = DAP.defaultNextResponse {
                DAP.seqNextResponse = resSeq
              , DAP.request_seqNextResponse = DAP.seqNextRequest req
              , DAP.successNextResponse = False
              , DAP.messageNextResponse = msg
              }

      U.addResponse $ NextResponse res

