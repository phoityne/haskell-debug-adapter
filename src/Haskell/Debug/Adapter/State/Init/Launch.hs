{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Haskell.Debug.Adapter.State.Init.Launch where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Except
import Control.Monad.State
import Control.Concurrent
import Control.Lens
import Text.Parsec
import qualified Text.Read as R
import qualified System.Log.Logger as L
import qualified Data.ByteString.Lazy as LB
import qualified Data.List as L
import qualified Data.Version as V
import qualified System.Directory as D

import qualified Haskell.DAP as DAP
import qualified Haskell.Debug.Adapter.Utility as U
import qualified Haskell.Debug.Adapter.State.Utility as SU
import Haskell.Debug.Adapter.Type
import Haskell.Debug.Adapter.Constant
import qualified Haskell.Debug.Adapter.Logger as L
import qualified Haskell.Debug.Adapter.GHCi as P

import qualified HIE.Bios as HIE
import qualified HIE.Bios.Types as HIE
import qualified HIE.Bios.Environment as HIE

-- |
--   Any errors should be critical. don't catch anything here.
--
instance StateActivityIF InitStateData DAP.LaunchRequest where
  action _ (LaunchRequest req) = do
    liftIO $ L.debugM _LOG_APP $ "InitState LaunchRequest called. " ++ show req
    app req

-- |
--   @see https://github.com/Microsoft/vscode/issues/4902
--   @see https://microsoft.github.io/debug-adapter-protocol/overview
--
app :: DAP.LaunchRequest -> AppContext (Maybe StateTransit)
app req = flip catchError errHdl $ do

  setUpConfig req
  setUpLogger req

  U.sendStdoutEvent "Configuration read.\n"
  U.sendConsoleEvent "Starting GHCi.\n"
  U.sendErrorEvent "Wait for a moment.\n\n"

  -- must start here. can not start in the entry of GHCiRun State.
  -- because there is a transition from DebugRun to GHCiRun.
  flags <- startGHCi req
  setPrompt
  launchCmd req
  setMainArgs
  loadStarupFile flags

  -- dont send launch response here.
  -- it must send after configuration done response.
  modify $ \s-> s{_launchReqSeqAppStores = DAP.seqLaunchRequest req}

  -- after initialized event, vscode send setBreak... and
  -- ConfigurationDone request.
  initSeq <- U.getIncreasedResponseSequence
  let reqSeq = DAP.seqLaunchRequest req

  U.addResponse $ InitializedEvent $ DAP.defaultInitializedEvent {DAP.seqInitializedEvent = initSeq, DAP.request_seqInitializedEvent = reqSeq}

  return $ Just Init_GHCiRun

  where
    -- |
    --
    errHdl :: String -> AppContext (Maybe StateTransit)
    errHdl msg = do
      liftIO $ L.errorM _LOG_APP msg
      resSeq <- U.getIncreasedResponseSequence
      let res = DAP.defaultLaunchResponse {
                DAP.seqLaunchResponse = resSeq
              , DAP.request_seqLaunchResponse = DAP.seqLaunchRequest req
              , DAP.successLaunchResponse = False
              , DAP.messageLaunchResponse = msg
              }

      U.addResponse $ LaunchResponse res
      return Nothing


-- |
--
setUpConfig :: DAP.LaunchRequest -> AppContext ()
setUpConfig req = do
  let args = DAP.argumentsLaunchRequest req
  appStores <- get

  let wsMVar = appStores^.workspaceAppStores
      ws = DAP.workspaceLaunchRequestArguments args
  _ <- liftIO $ takeMVar wsMVar
  liftIO $ putMVar wsMVar ws
  liftIO $ L.debugM _LOG_APP $ "workspace is " ++ ws

  let logPRMVar = appStores^.logPriorityAppStores
  logPR <- getLogPriority $ DAP.logLevelLaunchRequestArguments args
  _ <- liftIO $ takeMVar logPRMVar
  liftIO $ putMVar logPRMVar logPR

  put appStores {
      _startupAppStores     = U.replace [_SEP_WIN] [_SEP_UNIX] (DAP.startupLaunchRequestArguments args)
    , _startupFuncAppStores = maybe "" (\s->if null (U.strip s) then "" else  (U.strip s)) (DAP.startupFuncLaunchRequestArguments args)
    , _startupArgsAppStores = maybe "" (id) (DAP.startupArgsLaunchRequestArguments args)
    , _stopOnEntryAppStores = DAP.stopOnEntryLaunchRequestArguments args
    , _mainArgsAppStores    = maybe "" (id) (DAP.mainArgsLaunchRequestArguments args)
    }

  where
    getLogPriority logPRStr = case R.readEither logPRStr of
      Right lv -> return lv
      Left err -> do
        U.sendErrorEvent $ "log priority is invalid. WARNING set. [" ++ err ++ "]\n"
        return L.WARNING

-- |
--
setUpLogger :: DAP.LaunchRequest -> AppContext ()
setUpLogger req = do
  let args = DAP.argumentsLaunchRequest req
  ctx <- get
  logPR <- liftIO $ readMVar $ ctx^.logPriorityAppStores

  liftIO $ L.setUpLogger (DAP.logFileLaunchRequestArguments args) logPR


-- | Starts GHCi and returns the list of arguments it passed to invoke it.
startGHCi :: DAP.LaunchRequest -> AppContext [String]
startGHCi req = do
  let args = DAP.argumentsLaunchRequest req
      initPmpt = maybe _GHCI_PROMPT id (DAP.ghciInitialPromptLaunchRequestArguments args)
      envs = DAP.ghciEnvLaunchRequestArguments args

      -- Ignore ghciCmd LaunchRequestArguments
      -- Instead, use `hie-bios` to do the Right Thing across projects without complicated user input.
      -- Eventually, get rid of this option from haskell-dap.
      cmdStr = DAP.ghciCmdLaunchRequestArguments args
      (cmd:cmdOpts) = filter (not.null) $ U.split " " cmdStr

      startup_file = DAP.startupLaunchRequestArguments args

  appStores <- get
  cwd <- U.liftIOE $ readMVar $ appStores^.workspaceAppStores

  -- Use hie-bios when Cmd is exactly "ghci-dap"
  flags <- if cmdStr /= "ghci-dap" then addWithGHC cmdOpts else do
    isExist <- U.liftIOE $ D.doesFileExist startup_file
    when (False == isExist) $ do
      U.sendErrorEventLF $ "file not found. [" ++ startup_file ++ "]"
      -- throwError $ "file not found. [" ++ startup_file ++ "]"

    explicitCradle <- U.liftIOE $ HIE.findCradle startup_file
    cradle <- U.liftIOE $ maybe (HIE.loadImplicitCradle mempty startup_file)
                                (HIE.loadCradle mempty) explicitCradle

    libdir <- U.liftIOE (HIE.getRuntimeGhcLibDir cradle) >>= unwrapCradleResult "Failed to get runtime GHC libdir"

    -- getCompilerOptions depends on CWD being the proper root dir.
    let compilerOpts = D.withCurrentDirectory cwd $
#if MIN_VERSION_hie_bios(0,14,0)
                          HIE.getCompilerOptions startup_file HIE.LoadFile cradle
#else
                          HIE.getCompilerOptions startup_file [] cradle
#endif
    HIE.ComponentOptions {HIE.componentOptions = flags} <- U.liftIOE compilerOpts >>= unwrapCradleResult "Failed to get compiler options using hie-bios cradle"

    return $
#if __GLASGOW_HASKELL__ >= 913
      -- fwrite-if-simplified-core requires a recent bug fix regarding GHCi loading
      ["-fwrite-if-simplified-core"] ++
#endif
      ["--interactive", "-B"++libdir] ++ flags

  U.debugEV _LOG_APP $ show flags

  U.liftIOE $ L.debugM _LOG_APP $ "ghci initial prompt [" ++ initPmpt ++ "]."

  U.sendConsoleEventLF $ "CWD: " ++ cwd
  U.sendConsoleEventLF $ "CMD: " ++ L.intercalate " " (cmd:flags)
  U.sendConsoleEventLF ""

  P.startGHCi cmd flags cwd envs

  U.sendErrorEventLF $ "Now, waiting for an initial prompt(\""++initPmpt++"\")" ++ " from ghci."
  U.sendConsoleEventLF ""
  res <- P.expectInitPmpt initPmpt

  updateGHCiVersion res

  return flags

  where
    unwrapCradleResult m = \case
      HIE.CradleNone     -> panic (error m) "HIE.CradleNone"
      HIE.CradleFail err -> panic (error m) (unlines $ HIE.cradleErrorStderr err)
      HIE.CradleSuccess x -> return x

    panic exit m = do
      U.sendErrorEvent m
      exit

    updateGHCiVersion acc = case parse verParser "getGHCiVersion" (unlines acc) of
      Right v -> do
        U.debugEV _LOG_APP $ "GHCi version is " ++ V.showVersion v
        updateGHCiVersion' v
      Left e  -> do
        U.sendConsoleEventLF $ "Can not parse ghci version. [" ++ show e ++ "]. Assumes "  ++ V.showVersion _BASE_GHCI_VERSION ++ "."
        updateGHCiVersion' _BASE_GHCI_VERSION

    verParser = do
      _ <- manyTill anyChar (try (string "GHCi, version "))
      v1 <- manyTill digit (char '.')
      v2 <- manyTill digit (char '.')
      v3 <- manyTill digit (char ':')
      return $ V.makeVersion [read v1, read v2, read v3]

    updateGHCiVersion' v = do
      mver <- view ghciVerAppStores <$> get
      U.liftIOE $ putMVar mver v

-- |
--
setPrompt :: AppContext ()
setPrompt = do
  p <- view ghciPmptAppStores <$> get
  let pmpt = _DAP_CMD_END2 ++ "\\n" ++ p
      cmd  = ":set prompt \""++pmpt++"\""
      cmd2 = ":set prompt-cont \""++pmpt++"\""

  P.command cmd
  P.expectPmpt

  P.command cmd2
  P.expectPmpt

  return ()

-- |
--
launchCmd :: DAP.LaunchRequest -> AppContext ()
launchCmd req = do
  let args = DAP.argumentsLaunchRequest req
      dap = ":dap-launch "
      cmd = dap ++ U.showDAP args
      dbg = dap ++ show args

  P.command cmd
  U.debugEV _LOG_APP dbg
  P.expectPmpt

  return ()


-- |
--
setMainArgs :: AppContext ()
setMainArgs = view mainArgsAppStores <$> get >>= \case
  [] -> return ()
  args -> do
    let cmd  = ":set args "++args

    P.command cmd
    P.expectPmpt

    return ()


-- | Takes as an argument the list of flags used to invoke GHCi to determine
-- if the main module has already been loaded. If it hasn't, loads the main file.
loadStarupFile :: [String] -> AppContext ()
loadStarupFile flags = do
  file <- view startupAppStores <$> get
  when (not $ any (\lf -> lf `L.isSuffixOf` file) flags) $
    -- We only load the file if it hasn't already been given as an argument;
    -- Otherwise, we'll force loading the main module and all of its dependencies a second time.
    -- That is incredibly painful in large projects (like GHC).
    SU.loadHsFile file

  let cmd  = ":dap-context-modules "

  P.command cmd
  P.expectPmpt

  return ()

addWithGHC :: [String] -> AppContext [String]
addWithGHC [] = return []
addWithGHC cmds
  | L.elem "--with-ghc=haskell-dap" cmds = do
    U.infoEV _LOG_APP "can not use haskell-dap. deleting \"--with-ghc=haskell-dap\""
    addWithGHC $ L.delete "--with-ghc=haskell-dap" cmds
  | withGhciExists cmds = return cmds
  | "ghci" == head cmds = do
    U.infoEV _LOG_APP "\"--with-ghc\" option not found. adding \"--with-ghc=ghci-dap\""
    return $ head cmds:"--with-ghc=ghci-dap":tail cmds
  | otherwise = return cmds
  where
    withGhciExists [] = False
    withGhciExists (x:xs)
      | L.isPrefixOf "--with-ghc=" x = True
      | otherwise = withGhciExists xs

-- |
--
_TASKS_JSON_FILE_CONTENTS :: LB.ByteString
_TASKS_JSON_FILE_CONTENTS = U.str2lbs $ U.join "\n" $
  [
    "{"
  , "  // atuomatically created by phoityne-vscode"
  , "  "
  , "  \"version\": \"2.0.0\","
  , "  \"presentation\": {"
  , "    \"reveal\": \"always\","
  , "    \"panel\": \"new\""
  , "  },"
  , "  \"tasks\": ["
  , "    {"
  , "      \"group\": {"
  , "        \"kind\": \"build\","
  , "        \"isDefault\": true"
  , "      },"
  , "      \"label\": \"stack build\","
  , "      \"type\": \"shell\","
  , "      \"command\": \"echo START_STACK_BUILD && cd ${workspaceRoot} && stack build && echo END_STACK_BUILD \""
  , "    },"
  , "    { "
  , "      \"group\": \"build\","
  , "      \"type\": \"shell\","
  , "      \"label\": \"stack clean & build\","
  , "      \"command\": \"echo START_STACK_CLEAN_AND_BUILD && cd ${workspaceRoot} && stack clean && stack build && echo END_STACK_CLEAN_AND_BUILD \""
  , "    },"
  , "    { "
  , "      \"group\": {"
  , "        \"kind\": \"test\","
  , "        \"isDefault\": true"
  , "      },"
  , "      \"type\": \"shell\","
  , "      \"label\": \"stack test\","
  , "      \"command\": \"echo START_STACK_TEST && cd ${workspaceRoot} && stack test && echo END_STACK_TEST \""
  , "    },"
  , "    { "
  , "      \"isBackground\": true,"
  , "      \"type\": \"shell\","
  , "      \"label\": \"stack watch\","
  , "      \"command\": \"echo START_STACK_WATCH && cd ${workspaceRoot} && stack build --test --no-run-tests --file-watch && echo END_STACK_WATCH \""
  , "    }"
  , "  ]"
  , "}"
  ]

