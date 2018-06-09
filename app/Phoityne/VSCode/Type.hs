{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE Rank2Types         #-}

module Phoityne.VSCode.Type where

import Data.Data
import Control.Lens
import qualified Control.Exception.Safe as E

--------------------------------------------------------------------------------
-- | Command Line Argument Data Type

-- |
--  Help Exception
--
data HelpExitException = HelpExitException
  deriving (Show, Typeable)

instance E.Exception HelpExitException

-- |
--
data ArgData = ArgData {
  _hackageVersionArgData :: String
} deriving (Data, Typeable, Show, Read, Eq)

makeLenses ''ArgData

