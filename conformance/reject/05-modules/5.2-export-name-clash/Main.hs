-- Haskell 2010 Report 5.2: the unqualified names of exported entities must be distinct within their namespace
module Main (main, Lib.visible, Other.visible) where

import qualified Lib
import qualified Other

main :: IO ()
main = print Lib.visible
