{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.ErrorResponseBodyJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility
import Phoityne.VSCode.TH.MessageJSON

-- |
--    On error that is whenever 'success' is false, the body can provide more details. 
--
data ErrorResponseBody =
  ErrorResponseBody {
    errorErrorResponseBody :: Message  -- An optional, structured error message.
  } deriving (Show, Read, Eq)


$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "ErrorResponseBody") } ''ErrorResponseBody)

-- |
--
defaultErrorResponseBody :: ErrorResponseBody
defaultErrorResponseBody = ErrorResponseBody defaultMessage


