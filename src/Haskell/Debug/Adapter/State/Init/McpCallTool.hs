{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Haskell.Debug.Adapter.State.Init.McpCallTool where

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
instance StateActivityIF InitStateData MCP.McpCallToolRequest where
  action _ (McpCallToolRequest req) = do
    liftIO $ L.debugM _LOG_APP $ "McpInitState McpCallToolRequest called. " ++ show req
    app req

-- |
--
app :: MCP.McpCallToolRequest -> AppContext (Maybe StateTransit)
app req = do
  debugEV _LOG_APP "McpCallToolRequest called."
  stdioLogging $ str2lbs $ "[INFO] McpCallToolRequest called."++(show req)++"\n"

  let answer = if "dap-scope" == (MCP.nameMcpCallToolRequestParams (MCP.paramsMcpCallToolRequest req))
                 then "success: a is 3:integer."
                 else "success: void."
  let body = MCP.defaultMcpCallToolResultBody {
              MCP.contentMcpCallToolResultBody = [
                MCP.defaultMcpTextContent {
                  MCP.textMcpTextContent = answer
                }
              ]
            }
      res = MCP.defaultMcpCallToolResult {
            MCP.idMcpCallToolResult = MCP.idMcpCallToolRequest req
          , MCP.resultMcpCallToolResult = body
          }

  stdioLogging $ str2lbs $ "[INFO] McpCallToolResult:"++(show res)++"\n"
  U.addResponse $ McpCallToolResult res

  return Nothing



