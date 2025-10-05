module Main where

import System.Exit
import System.IO
import Control.Exception.Safe
import Options.Applicative
import Paths_haskell_debug_adapter (version)
import Data.Version (showVersion)

import Haskell.Debug.Adapter.Control


-- |
--  Main
--
main :: IO ()
main = getArgs >>= \args -> do
       flip catchAny exception
     $ flip finally  finalize
     $ run args stdin stdout

  where
    finalize = return ()
    exception e = print e >> exitFailure


-------------------------------------------------------------------------------
-- |
--   optparse-applicative
--
getArgs :: IO ArgData
getArgs = execParser parseInfo


-- |
--
parseInfo :: ParserInfo ArgData
parseInfo = info (helper <*> verOpt <*> options) $ mconcat
  [ fullDesc
  , header ""
  , footer ""
  , progDesc "Please see README.md"
  ]


-- |
--
verOpt :: Parser (a -> a)
verOpt = infoOption msg $ mconcat
  [ short 'v'
  , long  "version"
  , help  "Show version"
  ]
  where
    msg = "haskell-debug-adapter-" ++ showVersion version


-- |
--
options  :: Parser ArgData
options  = ArgData
  <$> hackageOption
  <*> stdioLogFileOption


-- |
--
hackageOption :: Parser (Maybe String)
hackageOption = optional $ strOption $ mconcat
  [ long "hackage-version"
  , help "hackage version"
  ]


-- |
--
stdioLogFileOption :: Parser (Maybe FilePath)
stdioLogFileOption = optional $ strOption $ mconcat
  [ long "stdio-log-file"
  , help "stdio log file"
  ]

