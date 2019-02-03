{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Haskell.Debug.Adapter.State.GHCiRun.SetExceptionBreakpoints where

import Control.Monad.IO.Class
import qualified System.Log.Logger as L

import qualified GHCi.DAP as DAP
import Haskell.Debug.Adapter.Type
import Haskell.Debug.Adapter.Constant
import qualified Haskell.Debug.Adapter.Utility as U
import qualified Haskell.Debug.Adapter.GHCi as P

instance StateRequestIF GHCiRunState DAP.SetExceptionBreakpointsRequest where
  --action :: (StateRequest s r) -> AppContext ()
  action (GHCiRun_SetExceptionBreakpoints req) = do
    liftIO $ L.debugM _LOG_APP $ "GHCiRunState SetExceptionBreakpointsRequest called. " ++ show req
    app req

-- |
--
app :: DAP.SetExceptionBreakpointsRequest -> AppContext (Maybe StateTransit)
app req = do
  let args = DAP.argumentsSetExceptionBreakpointsRequest req
      filters = DAP.filtersSetExceptionBreakpointsRequestArguments args

  mapM_ go $ getOptions filters

  resSeq <- U.getIncreasedResponseSequence
  let res = DAP.defaultSetExceptionBreakpointsResponse {
            DAP.seqSetExceptionBreakpointsResponse = resSeq
          , DAP.request_seqSetExceptionBreakpointsResponse = DAP.seqSetExceptionBreakpointsRequest req
          , DAP.successSetExceptionBreakpointsResponse = True
          }

  U.addResponse $ SetExceptionBreakpointsResponse res

  return Nothing
  
  where
    getOptions filters
      | null filters                      = ["-fno-break-on-exception", "-fno-break-on-error"]
      | filters == ["break-on-error"]     = ["-fno-break-on-exception", "-fbreak-on-error"]
      | filters == ["break-on-exception"] = ["-fbreak-on-exception",    "-fno-break-on-error"]
      | otherwise                         = ["-fbreak-on-exception",    "-fbreak-on-error" ] 

    go opt = do
      let cmd = ":set " ++ opt

      P.cmdAndOut cmd
      P.expect $ P.stdoutCallBk


