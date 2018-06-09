
{-# LANGUAGE MultiWayIf          #-}

module Phoityne.VSCode.Utility where

import Data.Either.Utils
import GHC.IO.Encoding
import Distribution.System
import Control.Monad.Trans.Resource
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.ConfigFile as CFG
import qualified Data.Tree as TR
import qualified Data.Conduit.Binary as C
import qualified Data.Conduit as C
import qualified Data.Conduit.List as C
import qualified Data.Char as CH


-- |
--
_SLASH :: Char
_SLASH = '/'


-- |
--
_BACK_SLASH :: Char
_BACK_SLASH = '\\'


-- |
--
toLower :: String -> String
toLower = map CH.toLower


-- |
--
toUpper :: String -> String
toUpper = map CH.toUpper


-- |
--
win2unixSlash :: String -> String
win2unixSlash = map (\c -> if c == _BACK_SLASH then _SLASH else c)


-- |
--
unix2winSlash :: String -> String
unix2winSlash = map (\c -> if c == _SLASH then _BACK_SLASH else c)


-- |
--   normalized path
--
nzPath :: FilePath -> FilePath
nzPath = drive2lower . win2unixSlash


-- |
--  to lowercase Windows drive letter 
-- 
drive2lower :: FilePath -> FilePath
drive2lower (x : ':' : xs) = CH.toLower x : ':' : xs
drive2lower xs = xs


-- |
--
str2bs :: String -> BS.ByteString
str2bs = TE.encodeUtf8 . T.pack


-- |
--
bs2str :: BS.ByteString -> String
bs2str = T.unpack. TE.decodeUtf8


-- |
--
str2lbs :: String -> LBS.ByteString
str2lbs = TLE.encodeUtf8 . TL.pack


-- |
--
lbs2str :: LBS.ByteString -> String
lbs2str = TL.unpack. TLE.decodeUtf8


-- |
--
getIniItems :: CFG.ConfigParser
            -> CFG.SectionSpec
            -> [(CFG.OptionSpec, String)]
getIniItems cp sec = foldr go [] opts
  where
    opts = forceEither $ CFG.options cp sec
    go opt acc = let value = forceEither $ CFG.get cp sec opt in
                 (opt, value) : acc


-- |
-- 
addChildTree :: TR.Tree a -> TR.Tree a -> TR.Tree a
addChildTree parent child = parent { TR.subForest = child : curForest}
  where curForest = TR.subForest parent


-- |
-- 
pushWithLimit :: [a] -> a -> Int -> [a]
pushWithLimit [] item _ = [item]
pushWithLimit buf item maxSize = if length buf > maxSize then item : L.init buf else item : buf


-- |
--
rdrop :: Int -> [a] -> [a]
rdrop cnt = reverse . drop cnt . reverse


-- |
--  
-- 
getReadHandleEncoding :: IO TextEncoding
getReadHandleEncoding = if
  | Windows == buildOS -> mkTextEncoding "CP932//TRANSLIT"
  | otherwise -> mkTextEncoding "UTF-8//TRANSLIT"


-- |
--  
-- 
loadFile :: FilePath -> IO BS.ByteString
loadFile path = do
  bs <- runResourceT
      $ C.sourceFile path
      C.$$ C.consume
  return $ BS.concat bs


-- |
--  
-- 
saveFile :: FilePath -> BS.ByteString -> IO ()
saveFile path cont = saveFileLBS path $ LBS.fromStrict cont


-- |
--  
-- 
saveFileLBS :: FilePath -> LBS.ByteString -> IO ()
saveFileLBS path cont = runResourceT
  $ C.sourceLbs cont
  C.$$ C.sinkFile path



