{-# LANGUAGE OverloadedStrings #-}

module Haskell.Debug.Adapter.Config (
  getConfigData
) where

import Haskell.Debug.Adapter.Type
import Haskell.Debug.Adapter.Utility

import Control.Lens
import Data.Yaml
import qualified Data.ByteString as BS

-- |
-- 
--
getConfigData :: ArgData -> IO ConfigData
getConfigData args = decodeEither' <$> getContent (args^.yamlFileArgData) >>= \case 
    Left  e -> error $ "invalid config file. [" ++ show e ++ "]"
    Right v -> return v

  where
    getContent Nothing  = return $ defaultYamlSetting
    getContent (Just f) = loadFile f
  
-- |
--  default ini setting
-- 
defaultYamlSetting :: BS.ByteString
defaultYamlSetting = str2bs $ unlines [
    "workDir  : \".\""
  , "logFile  : \"C:/work/haskell/haskell-debug-adapter/haskell-debug-adapter.log\""
  , "logLevel : \"DEBUG\""
  ]

