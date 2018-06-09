{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.StackFrameJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility
import Phoityne.VSCode.TH.SourceJSON

-- |
--   A Stackframe contains the source location.
--
data StackFrame =
  StackFrame {
    idStackFrame     :: Int     -- An identifier for the stack frame. This id can be used to retrieve the scopes of the frame with the 'scopesRequest'.
  , nameStackFrame   :: String  -- The name of the stack frame, typically a method name.
  , sourceStackFrame :: Source  -- The optional source of the frame.
  , lineStackFrame   :: Int     -- The line within the file of the frame. If source is null or doesn't exist, line is 0 and must be ignored.
  , columnStackFrame :: Int     -- The column within the line. If source is null or doesn't exist, column is 0 and must be ignored.
  , endLineStackFrame   :: Int  -- An optional end line of the range covered by the stack frame.
  , endColumnStackFrame :: Int  -- An optional end column of the range covered by the stack frame.
  } deriving (Show, Read, Eq)


$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "StackFrame") } ''StackFrame)
