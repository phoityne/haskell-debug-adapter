{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Haskell.Debug.Adapter.State.Utility where

import qualified System.Log.Logger as L
import qualified Text.Read as R
import Control.Monad.Except
import Control.Concurrent (threadDelay)

import qualified Haskell.DAP as DAP
import Haskell.Debug.Adapter.Type
import Haskell.Debug.Adapter.Constant
import qualified Haskell.Debug.Adapter.Utility as U
import qualified Haskell.Debug.Adapter.GHCi as P


-- |
--
setBreakpointsRequest :: DAP.SetBreakpointsRequest -> AppContext (Maybe StateTransit)
setBreakpointsRequest req = flip catchError errHdl $ do
  let args = DAP.argumentsSetBreakpointsRequest req
      dap = ":dap-set-breakpoints "
      cmd = dap ++ U.showDAP args
      dbg = dap ++ show args

  P.command cmd
  U.debugEV _LOG_APP dbg
  P.expectPmpt >>= takeDapResult >>= dapHdl

  return Nothing

  where
    -- |
    --
    dapHdl :: String -> AppContext ()
    dapHdl str = case R.readEither str of
      Left err -> errHdl err >> return ()
      Right (Left err) -> errHdl err >> return ()
      Right (Right body) -> do
        resSeq <- U.getIncreasedResponseSequence
        let res = DAP.defaultSetBreakpointsResponse {
                  DAP.seqSetBreakpointsResponse = resSeq
                , DAP.request_seqSetBreakpointsResponse = DAP.seqSetBreakpointsRequest req
                , DAP.successSetBreakpointsResponse = True
                , DAP.bodySetBreakpointsResponse = body
                }

        U.addResponse $ SetBreakpointsResponse res

    -- |
    --
    errHdl :: String -> AppContext (Maybe StateTransit)
    errHdl msg = do
      U.errorEV _LOG_APP msg
      resSeq <- U.getIncreasedResponseSequence
      let res = DAP.defaultSetBreakpointsResponse {
                DAP.seqSetBreakpointsResponse = resSeq
              , DAP.request_seqSetBreakpointsResponse = DAP.seqSetBreakpointsRequest req
              , DAP.successSetBreakpointsResponse = False
              , DAP.messageSetBreakpointsResponse = msg
              }

      U.addResponse $ SetBreakpointsResponse res
      return Nothing


-- |
--
setExceptionBreakpointsRequest :: DAP.SetExceptionBreakpointsRequest -> AppContext (Maybe StateTransit)
setExceptionBreakpointsRequest req = do
  let args = DAP.argumentsSetExceptionBreakpointsRequest req
      filters = DAP.filtersSetExceptionBreakpointsRequestArguments args

  mapM_ go $ getOptions filters

  resSeq <- U.getIncreasedResponseSequence
  let res = DAP.defaultSetExceptionBreakpointsResponse {
            DAP.seqSetExceptionBreakpointsResponse = resSeq
          , DAP.request_seqSetExceptionBreakpointsResponse = DAP.seqSetExceptionBreakpointsRequest req
          , DAP.successSetExceptionBreakpointsResponse = True
          }

  U.addResponse $ SetExceptionBreakpointsResponse res

  return Nothing

  where
    getOptions filters
      | null filters                      = ["-fno-break-on-exception", "-fno-break-on-error"]
      | filters == ["break-on-error"]     = ["-fno-break-on-exception", "-fbreak-on-error"]
      | filters == ["break-on-exception"] = ["-fbreak-on-exception",    "-fno-break-on-error"]
      | otherwise                         = ["-fbreak-on-exception",    "-fbreak-on-error" ]

    go opt = do
      let cmd = ":set " ++ opt

      P.command cmd
      P.expectPmpt



-- |
--
setFunctionBreakpointsRequest :: DAP.SetFunctionBreakpointsRequest -> AppContext (Maybe StateTransit)
setFunctionBreakpointsRequest req = flip catchError errHdl $ do
  let args = DAP.argumentsSetFunctionBreakpointsRequest req
      dap = ":dap-set-function-breakpoints "
      cmd = dap ++ U.showDAP args
      dbg = dap ++ show args

  P.command cmd
  U.debugEV _LOG_APP dbg
  P.expectPmpt >>= takeDapResult >>= dapHdl

  return Nothing

  where

    -- |
    --
    dapHdl :: String -> AppContext ()
    dapHdl str = case R.readEither str of
      Left err -> errHdl err >> return ()
      Right (Left err) -> errHdl err >> return ()
      Right (Right body) -> do
        resSeq <- U.getIncreasedResponseSequence
        let res = DAP.defaultSetFunctionBreakpointsResponse {
                  DAP.seqSetFunctionBreakpointsResponse = resSeq
                , DAP.request_seqSetFunctionBreakpointsResponse = DAP.seqSetFunctionBreakpointsRequest req
                , DAP.successSetFunctionBreakpointsResponse = True
                , DAP.bodySetFunctionBreakpointsResponse = body
                }

        U.addResponse $ SetFunctionBreakpointsResponse res

    -- |
    --
    errHdl :: String -> AppContext (Maybe StateTransit)
    errHdl msg = do
      U.errorEV _LOG_APP msg
      resSeq <- U.getIncreasedResponseSequence
      let res = DAP.defaultSetFunctionBreakpointsResponse {
                DAP.seqSetFunctionBreakpointsResponse = resSeq
              , DAP.request_seqSetFunctionBreakpointsResponse = DAP.seqSetFunctionBreakpointsRequest req
              , DAP.successSetFunctionBreakpointsResponse = False
              , DAP.messageSetFunctionBreakpointsResponse = msg
              }

      U.addResponse $ SetFunctionBreakpointsResponse res
      return Nothing



-- |
--
terminateGHCi :: AppContext ()
terminateGHCi = do
  let cmd = ":quit"

  P.command cmd
  P.expectPmpt
  return ()


-- |
--
evaluateRequest :: DAP.EvaluateRequest -> AppContext (Maybe StateTransit)
evaluateRequest req = do

  let args = DAP.argumentsEvaluateRequest req
      dap = ":dap-evaluate "
      cmd = dap ++ U.showDAP args
      dbg = dap ++ show args

  P.command cmd
  U.debugEV _LOG_APP dbg
  P.expectPmpt >>= takeDapResult >>= dapHdl

  return Nothing

  where

    -- |
    --
    dapHdl :: String -> AppContext ()
    dapHdl str = case R.readEither str of
      Left err -> errHdl $ err ++ " : " ++ str
      Right (Left err) -> errHdl err
      Right (Right body) -> do
        resSeq <- U.getIncreasedResponseSequence
        let res = DAP.defaultEvaluateResponse {
                  DAP.seqEvaluateResponse = resSeq
                , DAP.request_seqEvaluateResponse = DAP.seqEvaluateRequest req
                , DAP.successEvaluateResponse = True
                , DAP.bodyEvaluateResponse = body
                }

        U.addResponse $ EvaluateResponse res


    -- |
    --
    errHdl :: String -> AppContext ()
    errHdl msg = do
      U.sendErrorEventLF msg
      resSeq <- U.getIncreasedResponseSequence
      let res = DAP.defaultEvaluateResponse {
                DAP.seqEvaluateResponse = resSeq
              , DAP.request_seqEvaluateResponse = DAP.seqEvaluateRequest req
              , DAP.successEvaluateResponse = False
              , DAP.messageEvaluateResponse = msg
              }

      U.addResponse $ EvaluateResponse res

-- |
--
completionsRequest :: DAP.CompletionsRequest -> AppContext (Maybe StateTransit)
completionsRequest req = flip catchError errHdl $ do

  let args = DAP.argumentsCompletionsRequest req
      key  = DAP.textCompletionsRequestArguments args
      size = "0-50"
      cmd = ":complete repl " ++ size ++ " \"" ++ key ++ "\""

  P.command cmd
  outs <- P.expectPmpt

  resSeq <- U.getIncreasedResponseSequence
  let items = createItems outs
      body  = DAP.defaultCompletionsResponseBody {
              DAP.targetsCompletionsResponseBody = items
            }
      res = DAP.defaultCompletionsResponse {
            DAP.seqCompletionsResponse = resSeq
          , DAP.request_seqCompletionsResponse = DAP.seqCompletionsRequest req
          , DAP.successCompletionsResponse = True
          , DAP.bodyCompletionsResponse = body
          }

  U.addResponse $ CompletionsResponse res

  return Nothing

  where
    -- |
    --
    errHdl :: String -> AppContext (Maybe StateTransit)
    errHdl msg = do
      liftIO $ L.errorM _LOG_APP msg
      resSeq <- U.getIncreasedResponseSequence
      let res = DAP.defaultCompletionsResponse {
                DAP.seqCompletionsResponse = resSeq
              , DAP.request_seqCompletionsResponse = DAP.seqCompletionsRequest req
              , DAP.successCompletionsResponse = False
              , DAP.messageCompletionsResponse = msg
              }

      U.addResponse $ CompletionsResponse res
      return Nothing

    -- |
    --
    createItems :: [String] -> [DAP.CompletionsItem]
    createItems = map (createItem . normalize) . extracCompleteList

    -- |
    --
    createItem :: String -> DAP.CompletionsItem
    createItem (':':xs) = DAP.CompletionsItem xs
    createItem xs = DAP.CompletionsItem xs

    -- |
    --
    normalize :: String -> String
    normalize xs
      | 2 < length xs = tail . init $ xs
      | otherwise = xs

    -- |
    --
    extracCompleteList :: [String] -> [String]
    extracCompleteList [] = []
    extracCompleteList (_:[]) = []
    extracCompleteList (_:_:[]) = []
    extracCompleteList xs = tail . init $ xs



-- |
--
loadHsFile :: FilePath -> AppContext ()
loadHsFile file = do
  let cmd  = ":load "++ file

  P.command cmd
  P.expectPmpt

  return ()


-- |
--
terminateRequest :: DAP.TerminateRequest -> AppContext ()
terminateRequest req = do
  terminateGHCi

  liftIO $ threadDelay _1_SEC

  resSeq <- U.getIncreasedResponseSequence

  let res = DAP.defaultTerminateResponse {
            DAP.seqTerminateResponse         = resSeq
          , DAP.request_seqTerminateResponse = DAP.seqTerminateRequest req
          , DAP.successTerminateResponse     = True
          }

  U.addResponse $ TerminateResponse res
  U.sendTerminatedEvent
  U.sendExitedEvent

-- |
--
internalTerminateRequest :: AppContext ()
internalTerminateRequest = do

    terminateGHCi

    liftIO $ threadDelay _1_SEC

    U.sendTerminatedEvent
    U.sendExitedEvent


-- |
--
takeDapResult :: [String] -> AppContext String
takeDapResult res = case filter (U.startswith _DAP_HEADER) res of
  (x:[]) -> return $ drop (length _DAP_HEADER) x
  _ -> throwError $ "invalid dap result from ghci. " ++ show res


