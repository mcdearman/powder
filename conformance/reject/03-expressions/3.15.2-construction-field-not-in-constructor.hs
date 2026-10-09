-- Haskell 2010 Report 3.15.2 (REJECT): a construction may only use field labels declared with that constructor
module Main (main) where

data P = P { px :: Int }

data Q = Q { qx :: Int }

main :: IO ()
main = print (px (P { px = 1, qx = 2 }))
