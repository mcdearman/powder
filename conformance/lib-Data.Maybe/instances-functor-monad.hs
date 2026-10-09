-- Haskell 2010 Report 21 (Data.Maybe): Maybe as a Functor and Monad (fmap, >>=, >>, return, do notation),
-- together with the Data.Maybe eliminators
module Main (main) where

import Data.Maybe

phone :: [(String, Int)]
phone = [("ann", 1234), ("bob", 5678)]

owner :: [(Int, String)]
owner = [(1234, "Ann A."), (9999, "Zed Z.")]

roundTrip :: String -> Maybe String
roundTrip name = do
  number <- lookup name phone
  lookup number owner

addBoth :: String -> String -> Maybe Int
addBoth a b = do
  x <- lookup a phone
  y <- lookup b phone
  return (x + y)

main :: IO ()
main = do
  print ("fmap just", fmap (+ 1) (Just (1 :: Int)))
  print ("fmap nothing", fmap (+ 1) (Nothing :: Maybe Int))
  print ("fmap changes type", fmap show (Just (1 :: Int)))
  print ("bind", Just 3 >>= \x -> if x > 2 then Just (x * 2) else (Nothing :: Maybe Int))
  print ("bind fails", Just 1 >>= \x -> if x > 2 then Just (x * 2) else (Nothing :: Maybe Int))
  print ("bind on nothing never calls function", (Nothing :: Maybe Int) >>= (undefined :: Int -> Maybe Int))
  print ("then", Just 'a' >> Just 'b', (Nothing :: Maybe Char) >> Just 'b', Just 'a' >> (Nothing :: Maybe Char))
  print ("return", (return 'r' :: Maybe Char))
  print ("do success", roundTrip "ann")
  print ("do second lookup fails", roundTrip "bob")
  print ("do first lookup fails", roundTrip "cy")
  print ("do two lookups", addBoth "ann" "bob", addBoth "ann" "cy")
  print ("fromMaybe of do", fromMaybe "unknown" (roundTrip "bob"))
  print ("maybe of do", maybe 0 (* 2) (addBoth "ann" "ann"))
  print ("isJust of bind", isJust (lookup "ann" phone >>= (`lookup` owner)))
  print ("catMaybes of maps", catMaybes (map roundTrip ["ann", "bob", "ann"]))
  print ("mapMaybe with monadic function", mapMaybe (\n -> lookup n phone >>= \p -> if p > 2000 then Just p else Nothing) ["ann", "bob", "cy"])
  print ("sequence", sequence [Just 1, Just (2 :: Int)], sequence [Just 1, Nothing, Just (3 :: Int)])
  print ("mapM", mapM (`lookup` phone) ["ann", "bob"], mapM (`lookup` phone) ["ann", "cy"])
