module Haskell.Debug.Adapter.State.DebugRun.Source where

import Control.Monad.IO.Class
import qualified System.Log.Logger as L
import qualified Text.Read as R
import Control.Monad.Except


import qualified Haskell.DAP as DAP
import Haskell.Debug.Adapter.Type
import Haskell.Debug.Adapter.Constant
import qualified Haskell.Debug.Adapter.Utility as U
import qualified Haskell.Debug.Adapter.GHCi as P
import qualified Haskell.Debug.Adapter.State.Utility as SU



-- |
--  Any errors should be sent back as False result Response
--
instance StateActivityIF DebugRunStateData DAP.SourceRequest where
  action _ (SourceRequest req) = do
    liftIO $ L.debugM _LOG_APP $ "DebugRunState SourceRequest called. " ++ show req
    app req


-- |
--
app :: DAP.SourceRequest -> AppContext (Maybe StateTransit)
app req = do
  let args = DAP.argumentsSourceRequest req
      dap = ":dap-source "
      cmd = dap ++ U.showDAP args
      dbg = dap ++ show args

  P.command cmd
  U.debugEV _LOG_APP dbg
  P.expectPmpt >>= SU.takeDapResult >>= dapHdl

  return Nothing

  where

    -- |
    --
    dapHdl :: String -> AppContext ()
    dapHdl str = case R.readEither str of
      Left err -> errHdl $ err ++ " : " ++ str
      Right (Left err) -> errHdl err
      Right (Right body) -> do
        resSeq <- U.getIncreasedResponseSequence
        let res = DAP.defaultSourceResponse {
            DAP.seqSourceResponse = resSeq
          , DAP.request_seqSourceResponse = DAP.seqSourceRequest req
          , DAP.successSourceResponse = True
          , DAP.bodySourceResponse = body
          }

        U.addResponse $ SourceResponse res


    -- |
    --
    errHdl :: String -> AppContext ()
    errHdl msg = do
      U.sendErrorEventLF msg
      resSeq <- U.getIncreasedResponseSequence
      let res = DAP.defaultSourceResponse {
          DAP.seqSourceResponse = resSeq
        , DAP.request_seqSourceResponse = DAP.seqSourceRequest req
        , DAP.successSourceResponse = False
        , DAP.messageSourceResponse = msg
        }

      U.addResponse $ SourceResponse res

