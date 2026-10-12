{-# LANGUAGE ExplicitNamespaces #-}
-- ExplicitNamespaces: in a list of names imported or exported, `type` before a name says it is a type's
module Main (main) where

import Shapes (type Shape (..), type Named, area, type Size)
import Data.Maybe (type Maybe (Just, Nothing), fromMaybe)

describe :: Named -> Shape -> String
describe n s = n ++ ": " ++ show (area s :: Size)

main :: IO ()
main = do
  putStrLn (describe "circle" (Circle 2))
  putStrLn (describe "box" (Box 3 4))
  print (fromMaybe 0 (Just (1 :: Int)), fromMaybe 0 (Nothing :: Maybe Int))
