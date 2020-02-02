module Main where

import System.Exit
import System.IO
import Control.Exception.Safe
import Data.Default
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
parseInfo = info options $ mconcat
  [ fullDesc
  , header ""
  , footer ""
  , progDesc "Please see README.md"
  ]

-- |
--
options :: Parser ArgData
options = (<*>) helper
  $ def
  <$> verOption
  <*> hackageOption

-- |
--
verOption :: Parser (a -> a)
verOption = infoOption msg $ mconcat
  [ short 'v'
  , long  "version"
  , help  "Show version"
  ]
  where
    msg = "haskell-debug-adapter-" ++ showVersion version

-- |
--
hackageOption :: Parser (Maybe String)
hackageOption = optional $ strOption $ mconcat
  [ long "hackage-version"
  , help "hackage version"
  ]

