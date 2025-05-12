{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Haskell.Debug.Adapter.State.Init.McpInitialize where

import Control.Monad.IO.Class
import qualified System.Log.Logger as L

import Haskell.Debug.Adapter.Type
import Haskell.Debug.Adapter.Constant
import qualified Haskell.Debug.Adapter.Utility as U
import Haskell.Debug.Adapter.Utility
import qualified Haskell.Debug.Adapter.MCP.Type as MCP


-- |
--   Any errors should be critical. don't catch anything here.
--
instance StateActivityIF InitStateData MCP.McpInitializeRequest where
  action _ (McpInitializeRequest req) = do
    liftIO $ L.debugM _LOG_APP $ "McpInitState InitializeRequest called. " ++ show req
    app req

-- |
--
app :: MCP.McpInitializeRequest -> AppContext (Maybe StateTransit)
app req = do
  debugEV _LOG_APP "mcp initialize request called."
  stdioLogging $ str2lbs $ "[INFO] mcp initialize request called.\n"
  
  let body = MCP.defaultMcpInitializeResultBody {
               MCP.instructionsMcpInitializeResultBody = MCP.instruction
             }
      res = MCP.defaultMcpInitializeResult {
            MCP.idMcpInitializeResult = MCP.idMcpInitializeRequest req
          , MCP.resultMcpInitializeResult = body
          }
  stdioLogging $ str2lbs $ "[INFO] mcp initialize result."++(show res)++"\n"

  U.addResponse $ McpInitializeResult res

  return Nothing



