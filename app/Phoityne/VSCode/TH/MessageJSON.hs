{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.MessageJSON where

import Data.Aeson.TH
import qualified Data.Map as MAP

import Phoityne.VSCode.Utility

-- |
--   A structured message object. Used to return errors from requests.
--
data Message =
  Message {
    idMessage            :: Int                    -- Unique identifier for the message.
  , formatMessage        :: String                 -- A format string for the message. Embedded variables have the form '{name}'. If variable name starts with an underscore character, the variable does not contain user data (PII) and can be safely used for telemetry purposes. 
  , variablesMessage     :: MAP.Map String String  -- An object used as a dictionary for looking up the variables in the format string.  
  , sendTelemetryMessage :: Bool                   -- if true send to telemetry
  , showUserMessage      :: Bool                   -- if true show user
  , urlMessage           :: Maybe String           -- An optional url where additional information about this message can be found.
  , urlLabelMessage      :: Maybe String           -- An optional label that is presented to the user as the UI for opening the url. 
  } deriving (Show, Read, Eq)


$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "Message") } ''Message)

defaultMessage :: Message
defaultMessage = Message 0 "" (MAP.fromList []) False False Nothing Nothing

