
module Haskell.Debug.Adapter.MCP.Type (
    McpRequest(..)
  , defaultMcpRequest
  , McpInitializeRequest(..)
  , defaultMcpInitializeRequest
  , McpServerCapabilitiesTools(..)
  , defaultMcpServerCapabilitiesTools
  , McpServerCapabilities(..)
  , defaultMcpServerCapabilities
  , McpImplementation(..)
  , defaultMcpImplementation
  , McpInitializeResultBody(..)
  , defaultMcpInitializeResultBody
  , McpInitializeResult(..)
  , defaultMcpInitializeResult
  , McpInitializedNotification(..)
  , defaultMcpInitializedNotification
  , McpToolsListRequest(..)
  , defaultMcpToolsListRequest
  , McpKeyType(..)
  , defaultMcpKeyType
  , McpInputSchemaProperties(..)
  , defaultMcpInputSchemaPropertiesLaunch
  , defaultMcpInputSchemaPropertiesSetBreakpoint
  , defaultMcpInputSchemaPropertiesScope
  , McpInputSchema(..)
  , defaultMcpInputSchema
  , McpTool(..)
  , defaultMcpTool
  , McpListToolsResultBody(..)
  , defaultMcpListToolsResultBody
  , McpListToolsResult(..)
  , defaultMcpListToolsResult
  , RawJsonString(..)
  , McpCallToolRequest(..)
  , defaultMcpCallToolRequest
  , McpTextContent(..)
  , defaultMcpTextContent
  , McpCallToolResultBody(..)
  , defaultMcpCallToolResultBody
  , McpCallToolResult(..)
  , defaultMcpCallToolResult
  , McpCallToolRequestParams(..)
  , defaultMcpCallToolRequestParams
  , instruction
  , toolListDescription
) where


-- |
--   Client-initiated request
--   {"jsonrpc":"2.0","id":1,"method":"initialize",
--    "params":{"protocolVersion":"2024-11-05","capabilities":{"roots":{"listChanged":true}},"clientInfo":{"name":"Visual Studio Code","version":"1.99.2"}}}
--
data McpRequest =
  McpRequest {
    jsonrpcMcpRequest   :: String
  , idMcpRequest        :: Maybe Int
  , methodMcpRequest    :: String
  } deriving (Show, Read, Eq)


-- |
--
defaultMcpRequest :: McpRequest
defaultMcpRequest = McpRequest {
    jsonrpcMcpRequest     = ""
  , idMcpRequest    = Nothing
  , methodMcpRequest = ""
  }

-- |
--   Client-initiated request
--   {"jsonrpc":"2.0","id":1,"method":"initialize",
--    "params":{"protocolVersion":"2024-11-05","capabilities":{"roots":{"listChanged":true}},"clientInfo":{"name":"Visual Studio Code","version":"1.99.2"}}}
--
data McpInitializeRequest =
  McpInitializeRequest {
    jsonrpcMcpInitializeRequest   :: String
  , idMcpInitializeRequest        :: Int
  , methodMcpInitializeRequest    :: String
  -- , paramsMcpInitializeRequest :: Maybe String
  } deriving (Show, Read, Eq)


-- |
--
defaultMcpInitializeRequest :: McpInitializeRequest
defaultMcpInitializeRequest = McpInitializeRequest {
    jsonrpcMcpInitializeRequest     = ""
  , idMcpInitializeRequest    = 0
  , methodMcpInitializeRequest = ""
  -- , paramsMcpInitializeRequest = Nothing
  }


-- |
--
data McpServerCapabilitiesTools =
  McpServerCapabilitiesTools {
    listChangedMcpServerCapabilitiesTools :: Bool
  } deriving (Show, Read, Eq)


-- |
--
defaultMcpServerCapabilitiesTools :: McpServerCapabilitiesTools
defaultMcpServerCapabilitiesTools = McpServerCapabilitiesTools {
    listChangedMcpServerCapabilitiesTools = False
  }



-- |
--
data McpServerCapabilities =
  McpServerCapabilities {
    toolsMcpServerCapabilities :: McpServerCapabilitiesTools
  } deriving (Show, Read, Eq)


-- |
--
defaultMcpServerCapabilities :: McpServerCapabilities
defaultMcpServerCapabilities = McpServerCapabilities {
    toolsMcpServerCapabilities = defaultMcpServerCapabilitiesTools
  }



-- |
--
data McpImplementation =
  McpImplementation {
    nameMcpImplementation :: String
  , versionMcpImplementation :: String
  } deriving (Show, Read, Eq)


-- |
--
defaultMcpImplementation :: McpImplementation
defaultMcpImplementation = McpImplementation {
    nameMcpImplementation = "haskell-debug-adapter"
  , versionMcpImplementation = "0.0.43.0"
  }
  

-- |
--
data McpInitializeResultBody =
  McpInitializeResultBody {
    protocolVersionMcpInitializeResultBody :: String
  , capabilitiesMcpInitializeResultBody    :: McpServerCapabilities
  , serverInfoMcpInitializeResultBody :: McpImplementation
  , instructionsMcpInitializeResultBody  :: String
  } deriving (Show, Read, Eq)


-- |
--
defaultMcpInitializeResultBody :: McpInitializeResultBody
defaultMcpInitializeResultBody = McpInitializeResultBody {
    protocolVersionMcpInitializeResultBody = "2024-11-05"
  , capabilitiesMcpInitializeResultBody = defaultMcpServerCapabilities
  , serverInfoMcpInitializeResultBody = defaultMcpImplementation
  , instructionsMcpInitializeResultBody = ""
  }


-- |
--
data McpInitializeResult =
  McpInitializeResult {
    jsonrpcMcpInitializeResult   :: String
  , idMcpInitializeResult        :: Int
  , resultMcpInitializeResult :: McpInitializeResultBody
  } deriving (Show, Read, Eq)


-- |
--
defaultMcpInitializeResult :: McpInitializeResult
defaultMcpInitializeResult = McpInitializeResult {
    jsonrpcMcpInitializeResult     = "2.0"
  , idMcpInitializeResult    = 0
  , resultMcpInitializeResult = defaultMcpInitializeResultBody
  }


-- |
--
data McpInitializedNotification =
  McpInitializedNotification {
    jsonrpcMcpInitializedNotification   :: String
  } deriving (Show, Read, Eq)


-- |
--
defaultMcpInitializedNotification :: McpInitializedNotification
defaultMcpInitializedNotification = McpInitializedNotification {
    jsonrpcMcpInitializedNotification     = "2.0"
  }

--------------------------------------------------------------------------------------

-- |
--
newtype RawJsonString = RawJsonString { unRawJsonString :: String }
  deriving (Show, Read, Eq)
  
-- |
--
data McpToolsListRequest =
  McpToolsListRequest {
    jsonrpcMcpToolsListRequest   :: String
  , idMcpToolsListRequest        :: Int
  , methodMcpToolsListRequest    :: String
  -- , paramsMcpToolsListRequest :: Maybe String
  } deriving (Show, Read, Eq)


-- |
--
defaultMcpToolsListRequest :: McpToolsListRequest
defaultMcpToolsListRequest = McpToolsListRequest {
    jsonrpcMcpToolsListRequest     = ""
  , idMcpToolsListRequest    = 0
  , methodMcpToolsListRequest = ""
  -- , paramsMcpToolsListRequest = Nothing
  }


-- |
--
data McpKeyType =
  McpKeyType {
    typeMcpKeyType :: String
  } deriving (Show, Read, Eq)

-- |
--
defaultMcpKeyType :: McpKeyType
defaultMcpKeyType = McpKeyType {
    typeMcpKeyType = "String"
  }

-- |
--
data McpInputSchemaProperties =
  McpInputSchemaPropertiesLaunch {
    fileMcpInputSchemaProperties :: McpKeyType
  } |
  McpInputSchemaPropertiesSetBreakpoint {
    breakpointMcpInputSchemaProperties :: McpKeyType
  } |
  McpInputSchemaPropertiesScope {
    variableMcpInputSchemaProperties :: McpKeyType
  }deriving (Show, Read, Eq)

-- |
--
defaultMcpInputSchemaPropertiesLaunch :: McpInputSchemaProperties
defaultMcpInputSchemaPropertiesLaunch = McpInputSchemaPropertiesLaunch {
    fileMcpInputSchemaProperties = defaultMcpKeyType
  }

-- |
--
defaultMcpInputSchemaPropertiesSetBreakpoint :: McpInputSchemaProperties
defaultMcpInputSchemaPropertiesSetBreakpoint = McpInputSchemaPropertiesSetBreakpoint {
    breakpointMcpInputSchemaProperties = defaultMcpKeyType
  }

-- |
--
defaultMcpInputSchemaPropertiesScope :: McpInputSchemaProperties
defaultMcpInputSchemaPropertiesScope = McpInputSchemaPropertiesScope {
    variableMcpInputSchemaProperties = defaultMcpKeyType
  }

-- |
--
data McpInputSchema =
  McpInputSchema {
    typeMcpInputSchema :: String
  , propertiesMcpInputSchema :: McpInputSchemaProperties
  , requiredMcpInputSchema :: [String]
  } deriving (Show, Read, Eq)
 
-- |
--
defaultMcpInputSchema :: McpInputSchema
defaultMcpInputSchema = McpInputSchema {
    typeMcpInputSchema = "object"
  , propertiesMcpInputSchema = defaultMcpInputSchemaPropertiesLaunch
  , requiredMcpInputSchema = []
  }


-- |
--
data McpTool =
  McpTool {
    nameMcpTool :: String
  , descriptionMcpTool :: String
  , inputSchemaMcpTool :: McpInputSchema
  } deriving (Show, Read, Eq)


-- |
--
defaultMcpTool :: McpTool
defaultMcpTool = McpTool {
    nameMcpTool = ""
  , descriptionMcpTool = ""
  , inputSchemaMcpTool = defaultMcpInputSchema
  }



-- |
--
data McpListToolsResultBody =
  McpListToolsResultBody {
    toolsMcpListToolsResultBody :: RawJsonString
  } deriving (Show, Read, Eq)


-- |
--
defaultMcpListToolsResultBody :: McpListToolsResultBody
defaultMcpListToolsResultBody = McpListToolsResultBody {
    toolsMcpListToolsResultBody = RawJsonString ""
  }



-- |
--
data McpListToolsResult =
  McpListToolsResult {
    jsonrpcMcpListToolsResult   :: String
  , idMcpListToolsResult        :: Int
  , resultMcpListToolsResult :: McpListToolsResultBody
  } deriving (Show, Read, Eq)


-- |
--
defaultMcpListToolsResult :: McpListToolsResult
defaultMcpListToolsResult = McpListToolsResult {
    jsonrpcMcpListToolsResult     = "2.0"
  , idMcpListToolsResult    = 0
  , resultMcpListToolsResult = defaultMcpListToolsResultBody
  }


--------------------------------------------------------------------------------------
  
-- |
--
data McpCallToolRequestParams =
  McpCallToolRequestParams {
    nameMcpCallToolRequestParams   :: String
  , argumentsMcpCallToolRequestParams :: RawJsonString
  } deriving (Show, Read, Eq)


-- |
--
defaultMcpCallToolRequestParams :: McpCallToolRequestParams
defaultMcpCallToolRequestParams = McpCallToolRequestParams {
    nameMcpCallToolRequestParams     = ""
  , argumentsMcpCallToolRequestParams  = RawJsonString ""
  }

-- |
--
data McpCallToolRequest =
  McpCallToolRequest {
    jsonrpcMcpCallToolRequest   :: String
  , idMcpCallToolRequest        :: Int
  , methodMcpCallToolRequest    :: String
  , paramsMcpCallToolRequest :: McpCallToolRequestParams
  } deriving (Show, Read, Eq)


-- |
--
defaultMcpCallToolRequest :: McpCallToolRequest
defaultMcpCallToolRequest = McpCallToolRequest {
    jsonrpcMcpCallToolRequest     = ""
  , idMcpCallToolRequest    = 0
  , methodMcpCallToolRequest = ""
  , paramsMcpCallToolRequest = defaultMcpCallToolRequestParams
  }


-- |
--
data McpTextContent =
  McpTextContent {
    typeMcpTextContent :: String
  , textMcpTextContent :: String
  } deriving (Show, Read, Eq)

-- |
--
defaultMcpTextContent :: McpTextContent
defaultMcpTextContent = McpTextContent {
    typeMcpTextContent = "text"
  , textMcpTextContent = ""
  }

-- |
--
data McpCallToolResultBody =
  McpCallToolResultBody {
    contentMcpCallToolResultBody :: [McpTextContent]
  , isErrorMcpCallToolResultBody :: Bool
  } deriving (Show, Read, Eq)

-- |
--
defaultMcpCallToolResultBody :: McpCallToolResultBody
defaultMcpCallToolResultBody = McpCallToolResultBody {
    contentMcpCallToolResultBody = []
  , isErrorMcpCallToolResultBody = False
  }

-- |
--
data McpCallToolResult =
  McpCallToolResult {
    jsonrpcMcpCallToolResult   :: String
  , idMcpCallToolResult        :: Int
  , resultMcpCallToolResult :: McpCallToolResultBody
  } deriving (Show, Read, Eq)

-- |
--
defaultMcpCallToolResult :: McpCallToolResult
defaultMcpCallToolResult = McpCallToolResult {
    jsonrpcMcpCallToolResult     = "2.0"
  , idMcpCallToolResult    = 0
  , resultMcpCallToolResult = defaultMcpCallToolResultBody
  }



-- |
--
instruction :: String
instruction = "This toolkit provides a set of utilities for operating a Debug Adapter specifically designed for Haskell.\
\It enables debugging of applications built with Haskell. For details, refer to the description of each tool.\
\You must strictly follow the order and execute the tools in the exact sequence shown below.\
\  1. dap-initialize\
\  2. dap-launch\
\  3. dap-set-breakpoints\
\  4. dap-continue\
\  5. dap-stacktrace\
\  6. dap-scopes\
\  7. dap-variables\
\  8. dap-terminate\
\  9. dap-disconnect"


-- |
-- 
toolListDescription :: String
toolListDescription = "[\
\  {\
\    \"name\": \"dap-initialize\",\
\    \"description\": \"First, the MCP client sends this request to the MCP server. Then, the dap-launch request is sent.\",\
\    \"inputSchema\": {\
\      \"type\": \"object\",\
\      \"properties\": {\
\        \"adapterID\": {\
\          \"type\": \"string\",\
\          \"description\": \"The ID of the debug adapter.\"\
\        },\
\        \"linesStartAt1\": {\
\          \"type\": \"boolean\",\
\          \"description\": \"If true all line numbers are 1-based (default).\"\
\        },\
\        \"columnsStartAt1\": {\
\          \"type\": \"boolean\",\
\          \"description\": \"If true all column numbers are 1-based (default).\"\
\        },\
\        \"pathFormat\": {\
\          \"type\": \"string\",\
\          \"_enum\": [ \"path\", \"uri\" ],\
\          \"description\": \"Determines in what format paths are specified. The default is `path`, which is the native format.\"\
\        }\
\      },\
\      \"required\": [\
\        \"adapterID\",\
\        \"linesStartAt1\",\
\        \"columnsStartAt1\",\
\        \"pathFormat\"\
\      ]\
\    }\
\  },\
\  {\
\    \"name\": \"dap-launch\",\
\    \"description\": \"Upon completion of the dap-initialize request, the MCP client shall send this request to the MCP server. Subsequently, the dap-set-breakpoint request shall be sent.\",\
\    \"inputSchema\": {\
\      \"type\": \"object\",\
\      \"properties\": {\
\        \"name\": {\
\          \"type\": \"string\",\
\          \"description\": \"Phoityne specific argument. Must be `haskell-debug-adapter`.\"\
\        },\
\        \"type\": {\
\          \"type\": \"string\",\
\          \"description\": \"Phoityne specific argument. Must be `ghc`.\"\
\        },\
\        \"request\": {\
\          \"type\": \"string\",\
\          \"description\": \"Phoityne specific argument. Must be `launch`.\"\
\        },\
\        \"startup\": {\
\          \"type\": \"string\",\
\          \"description\": \"Phoityne specific argument. The path to debug start file.Must be a source file for the executable. (default: ${workspaceFolder}/app/Main.hs)\"\
\        },\
\        \"workspace\": {\
\          \"type\": \"string\",\
\          \"description\": \"Phoityne specific argument. The path to debugee workspace. (default: ${workspaceFolder})\"\
\        },\
\        \"logFile\": {\
\          \"type\": \"string\",\
\          \"description\": \"Phoityne specific argument. The path to the log file.(default: ${workspaceFolder}/.vscode/phoityne.log)\"\
\        },\
\        \"logLevel\": {\
\          \"type\": \"string\",\
\          \"description\": \"Phoityne specific argument. The Logging Priority.(default: WARNING)\"\
\        },\
\        \"ghciPrompt\": {\
\          \"type\": \"string\",\
\          \"description\": \"Phoityne specific argument. The ghci prompt used by hda.(default: H>>= )\"\
\        },\
\        \"ghciInitialPrompt\": {\
\          \"type\": \"string\",\
\          \"description\": \"Phoityne specific argument. The ghci initial prompt used by hda.(default: > )\"\
\        },\
\        \"ghciCmd\": {\
\          \"type\": \"string\",\
\          \"description\": \"Phoityne specific argument. The command to start debugging.(default: ghci-dap)\"\
\        },\
\        \"ghciEnv\": {\
\          \"type\": \"object\",\
\          \"properties\": {\
\            \"/\": {}\
\          },\
\          \"description\": \"Phoityne specific argument. required. Additional Environments while debugging.(default: `{}`)\"\
\        },        \
\        \"stopOnEntry\": {\
\          \"type\": \"boolean\",\
\          \"description\": \"Phoityne specific argument. Stop at the debugged function entry point.(default: false)\"\
\        }\
\      },\
\      \"required\": [\
\        \"name\",\
\        \"type\",\
\        \"request\",\
\        \"startup\",\
\        \"workspace\",\
\        \"logFile\",\
\        \"logLevel\",\
\        \"ghciPrompt\",\
\        \"ghciInitialPrompt\",\
\        \"ghciCmd\",\
\        \"ghciEnv\",\
\        \"stopOnEntry\"\
\      ]\
\    }\
\  },\
\  {\
\    \"name\": \"dap-set-breakpoints\",\
\    \"description\": \"After the completion of the dap-launch request, the MCP client must issue this request to the MCP server without exception. Subsequently, the dap-continue request must be issued.\",\
\    \"inputSchema\": {\
\      \"type\": \"object\",\
\      \"properties\": {\
\        \"source\": {\
\          \"type\": \"object\",\
\          \"properties\": {\
\            \"path\": {\
\              \"type\": \"string\",\
\              \"description\": \"The path of the source file.\"\
\            }\
\          },\
\          \"required\": [\
\            \"path\"\
\          ]\
\        },\
\        \"breakpoints\": {\
\          \"type\": \"array\",\
\          \"items\": {\
\            \"type\": \"object\",\
\            \"properties\": {\
\              \"line\": {\
\                \"type\": \"integer\",\
\                \"description\": \"The line number to set the breakpoint.\"\
\              }\
\            },\
\            \"required\": [\
\              \"line\"\
\            ]\
\          }\
\        }\
\      },\
\      \"required\": [\
\        \"source\",\
\        \"breakpoints\"\
\      ]\
\    }\
\  },\
\  {\
\    \"name\": \"dap-continue\",\
\    \"description\": \"After the completion of the dap-set-breakpoints request, the MCP client must issue this request to the MCP server without exception. Subsequently, the dap-stacktrace request must be issued.\",\
\    \"inputSchema\": {\
\      \"type\": \"object\",\
\      \"properties\": {\
\        \"threadId\": {\
\          \"type\": \"integer\",\
\          \"description\": \"Retrieve the stacktrace for this thread. (default 0)\"\
\        }\
\      },\
\      \"required\": [\
\        \"threadId\"\
\      ]\
\    }\
\  },\
\  {\
\    \"name\": \"dap-stacktrace\",\
\    \"description\": \"After the completion of the dap-continue request, the MCP client must issue this request to the MCP server without exception. Subsequently, the dap-scopes request must be issued.\",\
\    \"inputSchema\": {\
\      \"type\": \"object\",\
\      \"properties\": {\
\        \"threadId\": {\
\          \"type\": \"integer\",\
\          \"description\": \"Retrieve the stacktrace for this thread. (default 0)\"\
\        }\
\      },\
\      \"required\": [\
\        \"threadId\"\
\      ]\
\    }\
\  },\
\  {\
\    \"name\": \"dap-scopes\",\
\    \"description\": \"After the completion of the dap-stacktrace request, the MCP client must issue this request to the MCP server without exception. Subsequently, the dap-variables request must be issued.\",\
\    \"inputSchema\": {\
\      \"type\": \"object\",\
\      \"properties\": {\
\        \"frameId\": {\
\          \"type\": \"integer\",\
\          \"description\": \"The id of the StackFrame retrieved from the result of calling dap-stacktrace tool. (default 0)\"\
\        }\
\      },\
\      \"required\": [\
\        \"frameId\"\
\      ]\
\    }\
\  },\
\  {\
\    \"name\": \"dap-variables\",\
\    \"description\": \"After the completion of the dap-scopes request, the MCP client must issue this request to the MCP server without exception. Subsequently, the dap-disconnect request must be issued.\",\
\    \"inputSchema\": {\
\      \"type\": \"object\",\
\      \"properties\": {\
\        \"variablesReference\": {\
\          \"type\": \"integer\",\
\          \"description\": \"This variableReference is obtained from the result of calling dap-scopes tool and can be used to retrieve variables within the specified scope.(default 1)\"\
\        }\
\      },\
\      \"required\": [\
\        \"variablesReference\"\
\      ]\
\    }\
\  },\
\  {\
\    \"name\": \"dap-terminate\",\
\    \"description\": \"After the completion of the dap-variables request, the MCP client must issue this request to the MCP server without exception. Subsequently, the dap-disconnect request must be issued.\",\
\    \"inputSchema\": {\
\      \"type\": \"object\",\
\      \"properties\": {\
\        \"restart\": {\
\          \"type\": \"boolean\",\
\          \"description\": \"A value of true indicates that this terminate request is part of a restart sequence.\"\
\        }\
\      },\
\      \"required\": []\
\    }\
\  },\
\  {\
\    \"name\": \"dap-disconnect\",\
\    \"description\": \"After the completion of the dap-terminate request, the MCP client must issue this request to the MCP server without exception.\",\
\    \"inputSchema\": {\
\      \"type\": \"object\",\
\      \"properties\": {\
\        \"restart\": {\
\          \"type\": \"boolean\",\
\          \"description\": \"A value of true indicates that this disconnect request is part of a restart sequence.\"\
\        }\
\      },\
\      \"required\": []\
\    }\
\  }\
\]"


{-
-- {-# MultilineStrings #-} 
--
instruction :: String
instruction = """
This toolkit provides a set of utilities for operating a Debug Adapter specifically designed for Haskell.
It enables debugging of applications built with Haskell. For details, refer to the description of each tool.
You must strictly follow the order and execute the tools in the exact sequence shown below.
  1. dap-initialize
  2. dap-launch
  3. dap-set-breakpoints
  4. dap-continue
  5. dap-stacktrace
  6. dap-scopes
  7. dap-variables
  8. dap-terminate
  9. dap-disconnect
"""


-- |
-- {-# MultilineStrings #-} 
-- 
toolsJsonString :: String
toolsJsonString = """
[
  {
    "name": "dap-initialize",
    "description": "First, the MCP client sends this request to the MCP server. Then, the dap-launch request is sent.",
    "inputSchema": {
      "type": "object",
      "properties": {
        "adapterID": {
          "type": "string",
          "description": "The ID of the debug adapter."
        },
        "linesStartAt1": {
					"type": "boolean",
					"description": "If true all line numbers are 1-based (default)."
				},
				"columnsStartAt1": {
					"type": "boolean",
					"description": "If true all column numbers are 1-based (default)."
				},
        "pathFormat": {
					"type": "string",
					"_enum": [ "path", "uri" ],
					"description": "Determines in what format paths are specified. The default is `path`, which is the native format."
				}
      },
      "required": [
        "adapterID",
        "linesStartAt1",
        "columnsStartAt1",
        "pathFormat"
      ]
    }
  },
  {
    "name": "dap-launch",
    "description": "Upon completion of the dap-initialize request, the MCP client shall send this request to the MCP server. Subsequently, the dap-set-breakpoint request shall be sent.",
    "inputSchema": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string",
          "description": "Phoityne specific argument. Must be `haskell-debug-adapter`."
        },
        "type": {
          "type": "string",
          "description": "Phoityne specific argument. Must be `ghc`."
        },
        "request": {
          "type": "string",
          "description": "Phoityne specific argument. Must be `launch`."
        },
        "startup": {
          "type": "string",
          "description": "Phoityne specific argument. The path to debug start file.Must be a source file for the executable. (default: ${workspaceFolder}/app/Main.hs)"
        },
        "workspace": {
          "type": "string",
          "description": "Phoityne specific argument. The path to debugee workspace. (default: ${workspaceFolder})"
        },
        "logFile": {
          "type": "string",
          "description": "Phoityne specific argument. The path to the log file.(default: ${workspaceFolder}/.vscode/phoityne.log)"
        },
        "logLevel": {
          "type": "string",
          "description": "Phoityne specific argument. The Logging Priority.(default: WARNING)"
        },
        "ghciPrompt": {
          "type": "string",
          "description": "Phoityne specific argument. The ghci prompt used by hda.(default: H>>= )"
        },
        "ghciInitialPrompt": {
          "type": "string",
          "description": "Phoityne specific argument. The ghci initial prompt used by hda.(default: > )"
        },
        "ghciCmd": {
          "type": "string",
          "description": "Phoityne specific argument. The command to start debugging.(default: ghci-dap)"
        },
        "ghciEnv": {
          "type": "object",
          "properties": {
            "/": {}
          },
          "description": "Phoityne specific argument. required. Additional Environments while debugging.(default: `{}`)"
        },        
        "stopOnEntry": {
          "type": "boolean",
          "description": "Phoityne specific argument. Stop at the debugged function entry point.(default: false)"
        }
      },
      "required": [
        "name",
        "type",
        "request",
        "startup",
        "workspace",
        "logFile",
        "logLevel",
        "ghciPrompt",
        "ghciInitialPrompt",
        "ghciCmd",
        "ghciEnv",
        "stopOnEntry"
      ]
    }
  },
  {
    "name": "dap-set-breakpoints",
    "description": "After the completion of the dap-launch request, the MCP client must issue this request to the MCP server without exception. Subsequently, the dap-continue request must be issued.",
    "inputSchema": {
      "type": "object",
      "properties": {
        "source": {
          "type": "object",
          "properties": {
            "path": {
              "type": "string",
              "description": "The path of the source file."
            }
          },
          "required": [
            "path"
          ]
        },
        "breakpoints": {
          "type": "array",
          "items": {
            "type": "object",
            "properties": {
              "line": {
                "type": "integer",
                "description": "The line number to set the breakpoint."
              }
            },
            "required": [
              "line"
            ]
          }
        }
      },
      "required": [
        "source",
        "breakpoints"
      ]
    }
  },
  {
    "name": "dap-continue",
    "description": "After the completion of the dap-set-breakpoints request, the MCP client must issue this request to the MCP server without exception. Subsequently, the dap-stacktrace request must be issued.",
    "inputSchema": {
      "type": "object",
      "properties": {
        "threadId": {
          "type": "integer",
          "description": "Retrieve the stacktrace for this thread. (default 0)"
        }
      },
      "required": [
        "threadId"
      ]
    }
  },
  {
    "name": "dap-stacktrace",
    "description": "After the completion of the dap-continue request, the MCP client must issue this request to the MCP server without exception. Subsequently, the dap-scopes request must be issued.",
    "inputSchema": {
      "type": "object",
      "properties": {
        "threadId": {
          "type": "integer",
          "description": "Retrieve the stacktrace for this thread. (default 0)"
        }
      },
      "required": [
        "threadId"
      ]
    }
  },
  {
    "name": "dap-scopes",
    "description": "After the completion of the dap-stacktrace request, the MCP client must issue this request to the MCP server without exception. Subsequently, the dap-variables request must be issued.",
    "inputSchema": {
      "type": "object",
      "properties": {
        "frameId": {
          "type": "integer",
          "description": "The id of the StackFrame retrieved from the result of calling dap-stacktrace tool. (default 0)"
        }
      },
      "required": [
        "frameId"
      ]
    }
  },
  {
    "name": "dap-variables",
    "description": "After the completion of the dap-scopes request, the MCP client must issue this request to the MCP server without exception. Subsequently, the dap-disconnect request must be issued.",
    "inputSchema": {
      "type": "object",
      "properties": {
        "variablesReference": {
          "type": "integer",
          "description": "This variableReference is obtained from the result of calling dap-scopes tool and can be used to retrieve variables within the specified scope.(default 1)"
        }
      },
      "required": [
        "variablesReference"
      ]
    }
  },
  {
    "name": "dap-terminate",
    "description": "After the completion of the dap-variables request, the MCP client must issue this request to the MCP server without exception. Subsequently, the dap-disconnect request must be issued.",
    "inputSchema": {
      "type": "object",
      "properties": {
        "restart": {
          "type": "boolean",
          "description": "A value of true indicates that this terminate request is part of a restart sequence."
        }
      },
      "required": []
    }
  },
  {
    "name": "dap-disconnect",
    "description": "After the completion of the dap-terminate request, the MCP client must issue this request to the MCP server without exception.",
    "inputSchema": {
      "type": "object",
      "properties": {
        "restart": {
          "type": "boolean",
          "description": "A value of true indicates that this disconnect request is part of a restart sequence."
        }
      },
      "required": []
    }
  }
]
"""
-}