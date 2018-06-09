{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleContexts    #-}

module Phoityne.GHCi.Command (
    SourcePosition(..)
  , StackFrame(..)
  , BindingData(..)
  , start
  , quit
  , set
  , loadFile
  , loadModule
  , setBreak
  , setFuncBreak
  , delete
  , traceMain
  , traceFunc
  , trace
  , step
  , stepLocal
  , history
  , back
  , forward
  , bindings
  , force
  , info
  , showType
  , showKind
  , execBool
  , exec
  , complete
  , dapCommand
  , dapCommand2
  ) where

import Control.Concurrent
import Text.Parsec
import Data.Functor.Identity
import Data.Char
import qualified Data.List as L
import qualified Data.Map as M
import qualified System.Exit as S
import qualified Data.String.Utils as U
import qualified Data.Version as V

import Phoityne.GHCi.Process

-- |
--
_HS_FILE_EXT :: String
_HS_FILE_EXT = ".hs"

-- |
--
_DAP_HEADER :: String
_DAP_HEADER = "<<DAP>>"

-- |
--
type OutputHandler = String -> IO ()

-- |
--
type GHCiCommand = String

-- |
--
type GHCiOption = String

-- |
--
type ModuleName = String

-- |
--
type LineNo = Int

-- |
--
type ColNo = Int

-- |
--
type BreakId = Int

-- |
--
data SourcePosition = SourcePosition {
    filePathSourcePosition    :: FilePath
  , startLineNoSourcePosition :: Int
  , startColNoSourcePosition  :: Int
  , endLineNoSourcePosition   :: Int
  , endColNoSourcePosition    :: Int
  } deriving (Show, Read, Eq, Ord)


-- |
--
data StackFrame = StackFrame {
    idStackFrame       :: Int
  , functionStackFrame :: String
  , positionStackFrame :: SourcePosition
  } deriving (Show, Read, Eq, Ord)


-- |
--
data BindingData = BindingData {
    nameBindingData  :: String
  , typeBindingData  :: String
  , valueBindingData :: String
  } deriving (Show, Read, Eq, Ord)


-- |
--
start :: OutputHandler
      -> GHCiCommand
      -> [GHCiOption]
      -> FilePath
      -> String
      -> String
      -> M.Map String String
      -> IO (Either ErrorData GHCiProcess)
start outHdl cmd opts cwd initPmt pmt envs = do
  outHdl $ L.intercalate " " $ (cmd : opts) ++ ["in " ++ cwd, "\n"]
  runProcess cmd opts cwd pmt envs >>= withProcess
  where
    withProcess (Left err) = return $ Left err
    withProcess (Right ghci) = readCharWhile ghci (not.endOfStartMsg) >>= setupGHCi ghci

    setupGHCi _  (Left err) = return $ Left err
    setupGHCi ghci (Right msg) = do
      outHdl msg
      case getGHCiVersion msg of
        Right v -> setPrompt ghci{versionGHCiProcess = v}
        Left  m -> do
          outHdl "\n------------------------------------\n"
          outHdl m
          outHdl "\n------------------------------------\n"
          setPrompt ghci

    setPrompt ghci@(GHCiProcess _ _ _ _ pmt v _) = set ghci outHdl ("prompt \"" ++ pmt ++ "\"") >>= \case
      Left err -> return $ Left err
      Right _  -> if v >= (V.Version [8, 2, 0] []) then setPromptCont ghci else setPrompt2 ghci

    setPrompt2 ghci@(GHCiProcess _ _ _ _ pmt _ _) = set ghci outHdl ("prompt2 \"" ++ pmt ++ "\"") >>= \case
      Left err -> return $ Left err
      Right _  -> return $ Right ghci

    setPromptCont ghci@(GHCiProcess _ _ _ _ pmt _ _) = set ghci outHdl ("prompt-cont \"" ++ pmt ++ "\"") >>= \case
      Left err -> return $ Left err
      Right _  -> return $ Right ghci

    endOfStartMsg msg
      | U.endswith initPmt msg = True
      | endOfModLoadPrompt (last (lines msg)) = True
      | otherwise = False

    endOfModLoadPrompt str = case parse endOfModLoadPromptParser "endOfModLoadPrompt" str of
      Right _ -> True
      Left _  -> False

    endOfModLoadPromptParser = do
      char '*' >> manyTill anyChar (char '>') >> space >> eof
      return True

    getGHCiVersion str = case parse getGHCiVersionParser "getGHCiVersion" str of
      Right v -> Right v
      Left e  -> Left $ "can not parse ghci version. [" ++ show e ++ "] assumes "  ++ V.showVersion _BASE_GHCI_VERSION ++ "."

    getGHCiVersionParser = do
      _ <- manyTill anyChar (try (string "GHCi, version "))
      v1 <- manyTill digit (char '.')
      v2 <- manyTill digit (char '.')
      v3 <- manyTill digit (char ':')
      return $ V.makeVersion [read v1, read v2, read v3]

  
-- |
--
quit :: GHCiProcess -> OutputHandler -> IO (Either ErrorData S.ExitCode)
quit ghci outHdl = do
  let cmd = ":quit"
  outHdl $ cmd ++ "\n"

  writeLine ghci cmd >>= \case
    Left err -> return $ Left err
    Right _ -> readTillEOF ghci >>= \case
      Left err -> return $ Left err
      Right msg -> do
        outHdl msg
        exitProcess ghci


-- |
--
set :: GHCiProcess -> OutputHandler -> String -> IO (Either ErrorData ())
set ghci outHdl cmdArg = do
  let cmd = ":set " ++ cmdArg
  exec ghci outHdl cmd >>= \case
    Left err -> return $ Left err
    Right _  -> return $ Right ()

-- |
--
loadFile :: GHCiProcess
         -> OutputHandler
         -> FilePath
         -> IO (Either ErrorData [ModuleName])
loadFile ghci outHdl cmdArg = do
  let cmd = ":load " ++ (normalize cmdArg)
  outHdl $ cmd ++ "\n"

  lock ghci
  res <- writeLine ghci cmd >>= \case
    Left err -> return $ Left err
    Right _ -> readLineWhileIO ghci endOfLoadFile >>= withLoadResult
  unlock ghci

  return res
  where
    endOfLoadFile curStr _ = do
      outHdl $ curStr ++ "\n"
      if| U.startswith "Ok," curStr     -> return False
        | U.startswith "Failed," curStr -> return False
        | otherwise                     -> return True
   
    withLoadResult (Left err ) = return $ Left err
    withLoadResult (Right msges) = readTillPrompt ghci >>= \case
      Left err -> return $ Left err
      Right msg -> do
        outHdl msg
        return . Right $ foldr getModName [] msges

    -- |
    -- H>>= :load d:\haskell\unit-testing/test/Spec.hs
    -- [1 of 3] Compiling Lib              ( D:\haskell\unit-testing\src\Lib.hs, interpreted )
    -- [2 of 3] Compiling LibSpec          ( D:\haskell\unit-testing\test\LibSpec.hs, interpreted )
    -- [3 of 3] Compiling Main             ( D:\haskell\unit-testing\test\Spec.hs, interpreted )
    -- Ok, 3 modules loaded.
    -- H>>= 
    -- 
    getModName msg acc = case parse getModNameParser "getModNameParser" msg of
      Right mod -> mod:acc
      Left _    -> acc
      
    getModNameParser = 
      manyTill anyChar (try (string " Compiling ")) >> manyTill anyChar (char ' ')
 
    normalize path
      | True  == L.isPrefixOf "\"" path = path
      | False == L.isInfixOf  " "  path = path
      | otherwise = "\"" ++  (U.replace "\\" "\\\\" path) ++ "\""
      


-- |
--
loadModule :: GHCiProcess -> OutputHandler -> [ModuleName] -> IO (Either ErrorData ())
loadModule ghci outHdl mods = do
  let cmd = ":module + *" ++ U.join " *" mods

  lock ghci
  res <- exec ghci outHdl cmd >>= \case
    Left err -> return $ Left err
    Right _  -> return $ Right ()
  unlock ghci
    
  return res
  
-- |
--
setBreak :: GHCiProcess
         -> OutputHandler
         -> ModuleName
         -> LineNo
         -> ColNo
         -> IO (Either ErrorData (BreakId, SourcePosition))
setBreak ghci outHdl modName lineNo col = do
  let cmd = ":break " ++ modName ++ " " ++ show lineNo ++ (if (-1) == col then "" else " " ++ show col)

  lock ghci
  res <- exec ghci outHdl cmd >>= \case
    Left err -> return $ Left err
    Right msg -> getBreakId msg
  unlock ghci
    
  return res
  where
    getBreakId msg = case parse getBreakIdParser "getBreakId" msg of
      Right no  -> return $ Right no 
      Left  err -> return $ Left $ "unexpected break set result. " ++ show err ++ msg

    getBreakIdParser = do
      _   <- manyTill anyChar (string "Breakpoint ")
      no  <- manyTill digit (string " activated at ")
      pos <- parsePosition
      return (read no, pos)


-- |
--
setFuncBreak :: GHCiProcess
             -> OutputHandler
             -> String
             -> IO (Either ErrorData (BreakId, SourcePosition))
setFuncBreak ghci outHdl name = do
  let cmd = ":break " ++ name

  lock ghci
  res <- exec ghci outHdl cmd >>= \case
    Left err -> return $ Left err
    Right msg -> getBreakId msg
  unlock ghci
    
  return res
  where
    getBreakId msg = case parse getBreakIdParser "getBreakId" msg of
      Right no  -> return $ Right no 
      Left  err -> return $ Left $ "unexpected break set result. " ++ show err ++ msg

    getBreakIdParser = do
      _   <- manyTill anyChar (string "Breakpoint ")
      no  <- manyTill digit (string " activated at ")
      pos <- parsePosition
      return (read no, pos)

-- |
--
delete :: GHCiProcess -> OutputHandler -> BreakId -> IO (Either ErrorData ())
delete ghci outHdl bid = do
  let cmd = ":delete " ++ show bid

  lock ghci
  res <- exec ghci outHdl cmd >>= \case
    Left err -> return $ Left err
    Right _  -> return $ Right ()
  unlock ghci
    
  return res
-- |
--
traceMain :: GHCiProcess -> OutputHandler -> IO (Either ErrorData (Maybe SourcePosition))
traceMain ghci outHdl = traceFunc ghci outHdl "main" ""

-- |
--
traceFunc :: GHCiProcess -> OutputHandler -> String -> String -> IO (Either ErrorData (Maybe SourcePosition))
traceFunc ghci outHdl func args = do
  let cmd = ":trace " ++ func ++ if null args then "" else  " " ++ args
  exec ghci outHdl cmd >>= \case
    Left err  -> return $ Left err
    Right msg -> return $ mayException msg

-- |
--
trace :: GHCiProcess -> OutputHandler -> IO (Either ErrorData (Maybe SourcePosition))
trace ghci outHdl = do
  let cmd = ":trace"
  exec ghci outHdl cmd >>= \case
    Left err  -> return $ Left err
    Right msg -> return $ mayException msg

-- |
--
step :: GHCiProcess -> OutputHandler -> IO (Either ErrorData (Maybe SourcePosition))
step ghci outHdl = do
  let cmd = ":step"
  exec ghci outHdl cmd >>= \case
    Left err  -> return $ Left err
    Right msg -> return $ mayException msg

-- |
--
stepLocal :: GHCiProcess -> OutputHandler -> IO (Either ErrorData (Maybe SourcePosition))
stepLocal ghci outHdl = do
  let cmd = ":steplocal"
  exec ghci outHdl cmd >>= \case
    Left err  -> return $ Left err
    Right msg -> return $ mayException msg


-- |
--
mayException :: String -> Either ErrorData (Maybe SourcePosition)
mayException msg 
  | L.isInfixOf "Stopped in <exception thrown>" msg = Right Nothing
  | otherwise = case extractSourcePosition msg of
    Left err  -> Left err
    Right pos -> Right $ Just pos

-- |
--
history :: GHCiProcess -> OutputHandler -> Int -> IO (Either ErrorData [StackFrame])
history ghci outHdl size = do
  let cmd = ":history " ++ show size
  exec ghci outHdl cmd >>= \case
    Left err  -> return $ Left err
    Right msg -> return $ extractStackFrame msg



-- |
--
back :: GHCiProcess -> OutputHandler -> Int -> IO (Either ErrorData SourcePosition)
back ghci outHdl _ = do
  -- let cmd = ":back " ++ show val
  exec ghci outHdl ":back" >>= \case
    Left err  -> return $ Left err
    Right msg -> return $ extractTracePosition msg

-- |
--
forward :: GHCiProcess -> OutputHandler -> Int -> IO (Either ErrorData SourcePosition)
forward ghci outHdl _ = do
  -- let cmd = ":forward " ++ show val
  exec ghci outHdl ":forward" >>= \case
    Left err  -> return $ Left err
    Right msg -> return $ extractTracePosition msg

-- |
--
bindings :: GHCiProcess -> OutputHandler -> IO (Either ErrorData [BindingData])
bindings ghci outHdl = do
  let cmd = ":show bindings"
  exec ghci outHdl cmd >>= \case
    Left err  -> return $ Left err
    Right msg -> do
      let msgs = lines msg
      if 1 == length msgs then return $ Right []
        else return $ extractBindingBindingDatas $ unlines $ init $ msgs

-- |
--
force :: GHCiProcess -> OutputHandler -> String -> IO (Either ErrorData String)
force ghci outHdl target = do
  let cmd = ":force " ++ target
  exec ghci outHdl cmd >>= \case
    Left err  -> return $ Left err
    Right msg -> return $ extractErrorResult (normalizeConsoleOut msg) 


-- |
--
info :: GHCiProcess -> OutputHandler -> String -> IO (Either ErrorData String)
info ghci outHdl target = do
  let cmd = ":info " ++ target
  exec ghci outHdl cmd >>= \case
    Left err  -> return $ Left err
    Right msg -> return $ extractErrorResult (normalizeConsoleOut msg) 

-- |
--
showType :: GHCiProcess -> OutputHandler -> String -> IO (Either ErrorData String)
showType ghci outHdl target = do
  let cmd = ":type " ++ target
  exec ghci outHdl cmd >>= \case
    Left err  -> return $ Left err
    Right msg -> return $ extractErrorResult (normalizeConsoleOut msg) 

-- |
--
showKind :: GHCiProcess -> OutputHandler -> String -> IO (Either ErrorData String)
showKind ghci outHdl target = do
  let cmd = ":kind " ++ target
  exec ghci outHdl cmd >>= \case
    Left err  -> return $ Left err
    Right msg -> return $ extractErrorResult (normalizeConsoleOut msg) 

-- |
--
execBool :: GHCiProcess -> OutputHandler -> String -> IO (Either ErrorData Bool)
execBool ghci outHdl cmd = exec ghci outHdl cmd >>= \case
  Left err  -> return $ Left err
  Right msg -> return $ getConditionResult msg

  where
    getConditionResult :: String -> Either ErrorData Bool
    getConditionResult res
      | U.startswith "True"  res = Right True
      | U.startswith "False" res = Right False
      | otherwise = Left $ "invalid condition result. " ++ res
      
-- |
--  run command and return result string.
--
exec :: GHCiProcess -> OutputHandler -> String -> IO (Either ErrorData String)
exec ghci outHdl cmd = do
  outHdl $ cmd ++ "\n"

  writeLine ghci cmd >>= \case
    Left err -> return $ Left err
    Right _ -> readTillPrompt ghci >>= \case
      Left err -> return $ Left err
      Right msg -> do
        outHdl msg
        return $ Right msg

-- |
--
complete ::  GHCiProcess -> OutputHandler -> String -> Int -> IO (Either ErrorData [String])
complete ghci outHdl key size = do
  let cmd = ":complete repl 0-" ++ (show size) ++ " \"" ++ key ++ "\""
  exec ghci outHdl cmd >>= \case
    Left err  -> return $ Left err
    Right msg -> return $ Right $ map normalize $ extracCompleteList $ lines msg  

  where
    extracCompleteList [] = []
    extracCompleteList (_:[]) = []
    extracCompleteList (_:_:[]) = []
    extracCompleteList xs = tail . init $ xs

    normalize xs
      | 2 < length xs = tail . init $ xs
      | otherwise = xs

-- |
--   Private Utility
--

-- |
--
extractSourcePosition :: String -> (Either ErrorData SourcePosition)
extractSourcePosition src = case parse sourcePositionParser "extractSourcePosition" src of
  Right pos -> Right pos
  Left err  -> Left $ show err ++ " [INPUT]" ++ src

  where
    sourcePositionParser = try parse7 <|> try parse8

    parse7 = do
      _ <- manyTill anyChar (try (string "Stopped at "))
      parsePosition

    parse8 = do
      _ <- manyTill anyChar (try (string "Stopped in "))
      _ <- manyTill anyChar (try (string ", "))
      parsePosition

-- |
--  parser for
--   A) src\Phoityne\IO\Main.hs:31:11-14
--   B) src\Main.hs:(17,3)-(19,35)
--   C) src\Phoityne\IO\Main.hs:31:11
--      src\Phoityne\IO\Main.hs:31:11:
--
parsePosition :: forall u. ParsecT String u Identity SourcePosition
parsePosition = do
  path <- manyTill anyChar (try (string (_HS_FILE_EXT ++ ":")))
  (sl, sn, el, en) <- try parseA <|> try parseB <|> try parseC
  return $ SourcePosition (drive2lower path ++ _HS_FILE_EXT) sl sn el (en+1)
  where
    parseA = do
      ln <- manyTill digit (char ':')
      sn <- manyTill digit (char '-')
      en <- try (manyTill digit endOfLine) <|> try (manyTill digit eof)
      return ((read ln), (read sn), (read ln), (read en))

    parseB = do
      _ <- char '('
      sl <- manyTill digit (char ',')
      sn <- manyTill digit (char ')')
      _ <- string "-("
      el <- manyTill digit (char ',')
      en <- manyTill digit (char ')')
      return ((read sl), (read sn), (read el), (read en))

    parseC = do
      ln <- manyTill digit (char ':')
      sn <- try (manyTill digit (char ':')) <|> try (manyTill digit endOfLine) <|> try (manyTill digit eof)
      return ((read ln), (read sn), (read ln), (read sn))

    -- |
    --  to lowercase Windows drive letter 
    -- 
    drive2lower :: FilePath -> FilePath
    drive2lower (x : ':' : xs) = toLower x : ':' : xs
    drive2lower xs = xs

-- |
--
--  parser of
--    Phoityne>>= :history
--      -6  : spec (D:\haskell\unit-testing\test\LibSpec.hs:20:9-25)
--      -7  : spec (D:\haskell\unit-testing\test\LibSpec.hs:(34,7)-(35,26))
--
--
extractStackFrame :: String -> (Either ErrorData [StackFrame])
extractStackFrame src = go [] $ reverse $ filter (U.startswith "-") $ lines src
  where
    go acc [] = Right acc
    go acc (x:xs) = case parse stackFrameParser "extractStackFrame" (init x) of
      Left err  -> Left $ show err ++ " " ++ x
      Right dat -> go (dat:acc) xs

    stackFrameParser = do
      char '-'
      traceId  <- manyTill digit (many1 space >> char ':' >> space)
      funcName <- manyTill anyChar (space >> char '(')
      pos      <- parsePosition

      return $ StackFrame (read traceId) (removeColorCode funcName) pos

    removeColorCode str = case parse removeColorCodeParser "removeColorCode" str of
      Right res -> res
      Left _    -> str

    removeColorCodeParser = do
      let _esc_code = chr 27
      char _esc_code >> char '[' >> anyChar >> char 'm' 
      funcName <- manyTill anyChar (char _esc_code)
      return funcName

-- |
--
--  parser of
--    Phoityne>>= :back 10
--    Logged breakpoint at D:\haskell\unit-testing8\test\LibSpec.hs:(30,7)-(31,25)
--    Stopped at D:\haskell\unit-testing8\test\LibSpec.hs:31:9-25
--
extractTracePosition :: String -> (Either ErrorData SourcePosition)
extractTracePosition src = case parse extractTracePositionParser "extractTracePosition" src of
  Right pos -> Right pos
  Left err  -> Left $ show err ++ " [INPUT]" ++ src

  where
    extractTracePositionParser = try stopPos <|> try loggedPos

    stopPos = do
      _ <- manyTill anyChar (try (string "Stopped at "))
      parsePosition

    loggedPos = do
      _ <- manyTill anyChar (try (string "Logged breakpoint at "))
      parsePosition

-- |
--
--  parser of
--    Phoityne>>= :show bindings
--    _result ::
--       hspec-expectations-0.7.2:Test.Hspec.Expectations.Expectation = _
--    it :: [String] = []
--    Phoityne>>=
--
extractBindingBindingDatas :: String -> Either ErrorData [BindingData] 
extractBindingBindingDatas src = case parse bindingBindingDatasParser "extractBindingBindingDatas" src of
  Right vals -> Right . reverse $ vals
  Left err   -> Left $ show err ++ " [INPUT]" ++ src

  where
    bindingBindingDatasParser = do
      varName <- manyTill anyChar (try (string " ::"))
      bindingBindingDatasParser' (U.strip varName) []

    bindingBindingDatasParser' varName acc = do
      typeName <- manyTill anyChar (try (string " ="))
      try (hasMore varName  (U.strip typeName) acc) <|> lastItem  varName  (U.strip typeName) acc

    hasMore varName typeName acc = do
      str <- manyTill anyChar (try (string " ::"))
      let strs = lines str
      if 1 == length strs then return $ BindingData varName typeName (U.strip str) : acc
        else bindingBindingDatasParser' (U.strip (last strs))
          $ BindingData varName typeName (U.strip (U.join " " (init strs))) : acc

    lastItem varName typeName acc = do
      valStr <- manyTill anyChar eof
      return $ BindingData varName typeName (U.strip valStr) : acc

-- |
--
normalizeConsoleOut :: String -> String
normalizeConsoleOut = U.join " " . filter (not . U.startswith "***") . map U.strip . init . lines


-- |
--    Phoityne>>= :info IO xx
--
--      <interactive>:1:1: error: Not in scope: â€˜xxâ€™
--    Phoityne>>=
--
extractErrorResult :: String -> Either ErrorData String 
extractErrorResult str = case parse errorResultParser "extractErrorResult" str of
  Right errMsg -> Left errMsg
  Left  _      -> Right str
  where
    errorResultParser = do
      _ <- manyTill anyChar (try (string "<interactive>"))
      _ <- manyTill anyChar (char ' ')
      manyTill anyChar eof

-- |
--
lock ::  GHCiProcess -> IO ()
lock ghci = takeMVar (lockGHCiProcess ghci) >> return ()

-- |
--
unlock ::  GHCiProcess -> IO ()
unlock ghci = putMVar (lockGHCiProcess ghci) Lock



-- |
--
dapCommand :: GHCiProcess
           -> OutputHandler
           -> String
           -> String
           -> IO (Either ErrorData String)
dapCommand ghci outHdl cmdStr strDat = do
  let cmd = cmdStr ++ " " ++ strDat

  lock ghci
  res <- exec ghci outHdl cmd >>= \case
    Left err -> return $ Left err
    Right msg -> do
      let msgs = filter (L.isPrefixOf _DAP_HEADER) $ lines msg
      if 1 == length msgs then return $ Right $ drop (length _DAP_HEADER) $ head msgs
        else return $ Left $ "[dap] error. header " ++ _DAP_HEADER ++ "not found. " ++ msg 
  unlock ghci
    
  return res


-- |
--
dapCommand2 :: GHCiProcess
           -> OutputHandler
           -> String
           -> String
           -> IO (Either ErrorData [String])
dapCommand2 ghci outHdl cmdStr strDat = do
  let cmd = cmdStr ++ " " ++ strDat

  outHdl $ cmd ++ "\n"

  lock ghci
  res <- writeLine ghci cmd >>= \case
    Left err -> return $ Left err
    Right _ -> readLineWhileIO ghci proc >>= witResult
  unlock ghci

  return res
  where
    proc curStr _ = do
      outHdl curStr
      return $ curStr /= _DAP_CMD_END
   
    witResult (Left err ) = return $ Left err
    witResult (Right msges) = readTillPrompt ghci >>= \case
      Left err -> return $ Left err
      Right prmt -> do
        outHdl prmt
        return . Right $ msges


-- |
--
_DAP_CMD_END :: String
_DAP_CMD_END = "<<DAP_CMD_END>>"


