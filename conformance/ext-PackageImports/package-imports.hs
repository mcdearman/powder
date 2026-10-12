{-# LANGUAGE PackageImports #-}
-- PackageImports: an import may name the package its module is from
module Main (main) where

import "base" Data.Char (toUpper)
import qualified "base" Data.List as L
import "base" Data.Maybe

main :: IO ()
main = do
  putStrLn (map toUpper "shouted")
  print (L.sort [3, 1, 2 :: Int])
  print (fromMaybe 0 (Just (1 :: Int)))
