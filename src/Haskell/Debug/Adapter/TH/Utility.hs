
module Haskell.Debug.Adapter.TH.Utility where


-- |
--
rdrop :: String -> String -> String
rdrop str = reverse . drop (length str) . reverse

-- |
--
fieldModifier :: String -> String -> String
fieldModifier str  = tail . rdrop str
