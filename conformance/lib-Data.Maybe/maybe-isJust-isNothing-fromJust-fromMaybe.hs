-- Haskell 2010 Report 21 (Data.Maybe): Maybe(Nothing, Just), maybe, isJust, isNothing, fromJust (on Just
-- only), fromMaybe (lazy in its default)
module Main (main) where

import Data.Maybe

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)

describe :: Maybe Int -> String
describe Nothing = "nothing"
describe (Just n) = "just " ++ show n

main :: IO ()
main = do
  print ("constructors", Just 'x', (Nothing :: Maybe Char))
  print ("pattern matching", describe (Just 3), describe Nothing)
  print ("nested", Just (Just 1 :: Maybe Int), Just (Nothing :: Maybe Int), (Nothing :: Maybe (Maybe Int)))
  print ("Just is a function", map Just "ab")
  print ("maybe on Just", maybe 0 (+ 1) (Just 5 :: Maybe Int))
  print ("maybe on Nothing", maybe 0 (+ 1) (Nothing :: Maybe Int))
  print ("maybe changes type", maybe "none" show (Just 42 :: Maybe Int), maybe "none" show (Nothing :: Maybe Int))
  print ("maybe default is lazy", maybe undefined (* 2) (Just 4 :: Maybe Int))
  print ("maybe function is lazy", maybe 'd' undefined (Nothing :: Maybe Int))
  print ("maybe with safeDiv", maybe (-1) id (safeDiv 10 2), maybe (-1) id (safeDiv 10 0))
  print ("isJust", isJust (Just 'x'), isJust (Nothing :: Maybe Char))
  print ("isJust does not force contents", isJust (Just (undefined :: Int)))
  print ("isNothing", isNothing (Just 'x'), isNothing (Nothing :: Maybe Char))
  print ("isNothing is not isJust", all (\m -> isNothing m /= isJust m) [Just 1, Nothing, Just (2 :: Int)])
  print ("isJust nested", isJust (Just (Nothing :: Maybe Int)))
  print ("fromJust", fromJust (Just 'x'))
  print ("fromJust nested", fromJust (fromJust (Just (Just (7 :: Int)))))
  print ("fromJust of computed", fromJust (safeDiv 9 3))
  print ("fromJust . Just", map (fromJust . Just) [1, 2, 3 :: Int])
  print ("fromMaybe on Just", fromMaybe 0 (Just 5 :: Maybe Int))
  print ("fromMaybe on Nothing", fromMaybe 0 (Nothing :: Maybe Int))
  print ("fromMaybe default is lazy", fromMaybe undefined (Just 'k'))
  print ("fromMaybe string", fromMaybe "default" (lookup 2 [(1, "one")]), fromMaybe "default" (lookup 1 [(1 :: Int, "one")]))
  print ("fromMaybe is maybe id", all (\m -> fromMaybe 0 m == maybe 0 id m) [Just 1, Nothing, Just (2 :: Int)])
  print ("fromMaybe chained", fromMaybe 0 (safeDiv 1 0) + fromMaybe 0 (safeDiv 8 2))
  print ("eq", Just 1 == Just (1 :: Int), Just 1 == (Nothing :: Maybe Int), (Nothing :: Maybe Int) == Nothing)
  print ("ord Nothing is least", (Nothing :: Maybe Int) < Just minBound, compare (Just 1) (Just (2 :: Int)), maximum [Just 1, Nothing, Just (3 :: Int)])
  print ("show", show (Just (-1) :: Maybe Int), show (Just (Just "s")), showsPrec 11 (Just 'c') "")
  print ("read", (read "Just 5" :: Maybe Int), (read "Nothing" :: Maybe Int), (read "Just (Just (-2))" :: Maybe (Maybe Int)))
