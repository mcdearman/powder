{-# LANGUAGE ImportQualifiedPost #-}
-- ImportQualifiedPost: the word qualified may come after the module's name
module Main (main) where

import Data.Char qualified
import Data.List qualified as L
import Data.Maybe qualified as M (fromMaybe, isJust)
import qualified Data.Ratio as R

main :: IO ()
main = do
  print (Data.Char.toUpper 'a', L.sort "hello", M.fromMaybe 0 (Just (3 :: Int)), M.isJust (Nothing :: Maybe ()))
  print (R.numerator (3 R.% 6 :: Rational))
