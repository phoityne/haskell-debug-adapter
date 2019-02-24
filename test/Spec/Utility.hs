
module Spec.Utility where

--import Control.Lens
import qualified System.IO as S
import qualified System.Process as S
import qualified Data.ByteString.Lazy as B
import Control.Concurrent.MVar
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

--import qualified System.Log.Logger as L
--import qualified System.Log.Formatter as L
--import qualified System.Log.Handler as LH
--import qualified System.Log.Handler.Simple as LHS


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
str2lbs :: String -> B.ByteString
str2lbs = TLE.encodeUtf8 . TL.pack

-- |
--
lbs2str :: B.ByteString -> String
lbs2str = TL.unpack. TLE.decodeUtf8

-- |
--
createPipe :: IO (S.Handle, S.Handle)
createPipe = do
  (rHdl, wHdl) <- S.createPipe

  S.hSetBuffering rHdl S.NoBuffering
  S.hSetEncoding rHdl S.utf8
  S.hSetNewlineMode rHdl $ S.NewlineMode S.CRLF S.LF

  S.hSetBuffering wHdl S.NoBuffering
  S.hSetEncoding wHdl S.utf8
  S.hSetNewlineMode wHdl $ S.NewlineMode S.CRLF S.LF

  return (rHdl, wHdl) 


-- |
--
read2buf :: S.Handle -> MVar B.ByteString -> IO ()
read2buf hdl bufMVar = do
  s <- B.hGet hdl 1
  buf <- takeMVar bufMVar
  putMVar bufMVar $ buf `B.append` s
  read2buf hdl bufMVar

-- |
--
request :: S.Handle -> B.ByteString -> IO ()
request hdl str = do
  B.hPut hdl $ str2lbs $ _CONTENT_LENGTH ++ (show (B.length str))
  B.hPut hdl $ str2lbs _TWO_CRLF
  B.hPut hdl str
  S.hFlush hdl
