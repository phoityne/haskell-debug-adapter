{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.ScopesBodyJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility
import Phoityne.VSCode.TH.ScopeJSON

-- |
--   Response to "scopes" request.
--
data ScopesBody =
  ScopesBody {
    scopesScopesBody :: [Scope]  -- The scopes of the stackframe. If the array has length zero, there are no scopes available.
  } deriving (Show, Read, Eq)


$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "ScopesBody") } ''ScopesBody)

defaultScopesBody :: ScopesBody
defaultScopesBody = ScopesBody [Scope "GHCi scope" 1 Nothing Nothing False]

