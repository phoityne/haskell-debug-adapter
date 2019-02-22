{-# LANGUAGE OverloadedStrings #-}

module Haskell.Debug.Adapter.Constant where

import qualified Data.Version as V

-- |
--
_INI_SEC_DEF :: String
_INI_SEC_DEF = "DEFAULT"

-- |
--
_INI_DEF_WORK_DIR :: String
_INI_DEF_WORK_DIR  = "work_dir"

-- |
--
_INI_SEC_LOG :: String
_INI_SEC_LOG   = "LOG"

-- |
--
_INI_LOG_FILE :: String
_INI_LOG_FILE  = "file"

-- |
--
_INI_LOG_LEVEL :: String
_INI_LOG_LEVEL = "level"

-- |
--
_LOG_NAME :: String
_LOG_NAME = "HDA"


-- |
--
_LOG_REQUEST :: String
_LOG_REQUEST = "REQUEST"


-- |
--
_LOG_RESPONSE :: String
_LOG_RESPONSE = "RESPONSE"


-- |
--
_LOG_APP :: String
_LOG_APP = "APP"


-- |
--
_LOG_THREAD_MGR :: String
_LOG_THREAD_MGR = "THREAD_MGR"


-- |
--
_LOG_EVENT :: String
_LOG_EVENT = "EVENT"


-- |
--
_LOG_GHCI_STDOUT :: String
_LOG_GHCI_STDOUT = "GHCI_STDOUT"


-- |
--
_LOG_WATCH :: String
_LOG_WATCH = "WATCH"


-- |
--
_LOG_FORMAT :: String
_LOG_FORMAT = "$time [$pid($tid)] $prio $loggername - $msg"

-- |
--
_LOG_FORMAT_DATE :: String
_LOG_FORMAT_DATE = "%Y-%m-%d %H:%M:%S.%q"
-- _LOG_FORMAT_DATE = "%Y-%m-%d %H:%M:%S"


-- |
-- 
_CONTENT_LENGTH :: String
_CONTENT_LENGTH = "Content-Length: " 


-- |
--
_TWO_CRLF :: String
_TWO_CRLF = "\r\n\r\n"

-- |
--
_1_MILLI_SEC :: Int
_1_MILLI_SEC = 1 * 1000

-- |
--
_10_MILLI_SEC :: Int
_10_MILLI_SEC = 10 * 1000


-- |
--
_100_MILLI_SEC :: Int
_100_MILLI_SEC = 100 * 1000


-- |
--
_1_SEC :: Int
_1_SEC = 1000 * 1000

-- |
--
--
_SEP_WIN :: Char
_SEP_WIN = '\\'

-- |
--
--
_SEP_UNIX :: Char
_SEP_UNIX = '/'

-- |
--
_GHCI_PROMPT :: String
_GHCI_PROMPT = "Prelude> "

-- |
--
_GHCI_PROMPT_HDA :: String
_GHCI_PROMPT_HDA = "H>>= "


-- |
--
--_LF_WORD :: Word8
--_LF_WORD = '\n'

-- |
--
_LF_STR :: String
_LF_STR = "\n"


-- |
--
_BASE_GHCI_VERSION :: V.Version
_BASE_GHCI_VERSION =  V.Version [8, 0, 0] []


-- |
--
_DAP_HEADER :: String
_DAP_HEADER = "<<DAP>>"


-- |
--
_DAP_HEADER_OUTPUT_EVENT :: String
_DAP_HEADER_OUTPUT_EVENT = "<<DAP_OUTPUT_EVENT>>"


-- |
--
_DAP_CMD_END2 :: String
_DAP_CMD_END2 = "<<DAP_CMD_END2>>"


-- |
--
_HS_FILE_EXT :: String
_HS_FILE_EXT = ".hs"
