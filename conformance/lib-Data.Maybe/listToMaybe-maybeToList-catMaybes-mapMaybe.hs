-- Haskell 2010 Report 21 (Data.Maybe): listToMaybe, maybeToList, catMaybes, mapMaybe
module Main (main) where

import Data.Maybe

readDigit :: Char -> Maybe Int
readDigit c
  | c >= '0' && c <= '9' = Just (fromEnum c - fromEnum '0')
  | otherwise = Nothing

half :: Int -> Maybe Int
half n = if even n then Just (n `div` 2) else Nothing

main :: IO ()
main = do
  print ("listToMaybe", listToMaybe [1, 2, 3 :: Int])
  print ("listToMaybe empty", listToMaybe ([] :: [Int]))
  print ("listToMaybe singleton", listToMaybe "x")
  print ("listToMaybe infinite", listToMaybe [5 :: Int ..])
  print ("listToMaybe ignores tail", listToMaybe (1 : undefined :: [Int]))
  print ("listToMaybe does not force the head", isJust (listToMaybe [undefined :: Int]))
  print ("maybeToList", maybeToList (Just 'x'))
  print ("maybeToList nothing", maybeToList (Nothing :: Maybe Char))
  print ("maybeToList . listToMaybe", maybeToList (listToMaybe "abc"))
  print ("listToMaybe . maybeToList", listToMaybe (maybeToList (Just 'q')), listToMaybe (maybeToList (Nothing :: Maybe Char)))
  print ("maybeToList nested", maybeToList (Just (Nothing :: Maybe Int)))
  print ("catMaybes", catMaybes [Just 1, Nothing, Just 3, Nothing, Nothing, Just (6 :: Int)])
  print ("catMaybes all nothing", catMaybes [Nothing, Nothing :: Maybe Int])
  print ("catMaybes all just", catMaybes [Just 'a', Just 'b'])
  print ("catMaybes empty", catMaybes ([] :: [Maybe Int]))
  print ("catMaybes infinite", take 3 (catMaybes (cycle [Nothing, Just 'x'])))
  print ("catMaybes keeps order and duplicates", catMaybes [Just 2, Just 1, Nothing, Just (2 :: Int)])
  print ("catMaybes is concatMap maybeToList", catMaybes [Just 1, Nothing, Just (2 :: Int)] == concatMap maybeToList [Just 1, Nothing, Just 2])
  print ("catMaybes does not force contents", length (catMaybes [Just undefined, Nothing, Just (undefined :: Int)]))
  print ("mapMaybe", mapMaybe readDigit "a1b2c3")
  print ("mapMaybe none", mapMaybe readDigit "abc")
  print ("mapMaybe all", mapMaybe readDigit "987")
  print ("mapMaybe empty", mapMaybe readDigit "")
  print ("mapMaybe half", mapMaybe half [1 .. 10])
  print ("mapMaybe Just is identity", mapMaybe Just "same")
  print ("mapMaybe const Nothing", mapMaybe (const (Nothing :: Maybe Int)) "gone")
  print ("mapMaybe infinite", take 4 (mapMaybe half [1 ..]))
  print ("mapMaybe is catMaybes . map", mapMaybe half [1 .. 10] == catMaybes (map half [1 .. 10]))
  print ("mapMaybe with lookup", mapMaybe (`lookup` [(1 :: Int, "one"), (3, "three")]) [1, 2, 3, 4])
  print ("mapMaybe repeated halving", mapMaybe (\n -> half n >>= half) [4, 6, 8, 10, 12])
