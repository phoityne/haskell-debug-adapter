{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Haskell.Debug.Adapter.State.Init.McpToolsList where

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
instance StateActivityIF InitStateData MCP.McpToolsListRequest where
  action _ (McpToolsListRequest req) = do
    liftIO $ L.debugM _LOG_APP $ "McpInitState McpToolsListRequest called. " ++ show req
    app req

-- |
--
app :: MCP.McpToolsListRequest -> AppContext (Maybe StateTransit)
app req = do
  debugEV _LOG_APP "McpToolsListRequest called."
  stdioLogging $ str2lbs $ "[INFO] McpToolsListRequest called.\n"

  let body = MCP.defaultMcpListToolsResultBody {
             MCP.toolsMcpListToolsResultBody = MCP.RawJsonString MCP.toolListDescription
           }
      res = MCP.defaultMcpListToolsResult {
            MCP.idMcpListToolsResult = MCP.idMcpToolsListRequest req
          , MCP.resultMcpListToolsResult = body
          }
  stdioLogging $ str2lbs $ "[INFO] McpListToolsResult."++(show res)++"\n"

  U.addResponse $ McpListToolsResult res

  return Nothing

