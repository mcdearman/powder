{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE PartialTypeSignatures, NamedWildCards, OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
-- PartialTypeSignatures and NamedWildCards: a type with parts left out, to be found; and OverloadedStrings: a string
-- written in a program is of any type that has them
module Main (main) where

import Data.String (IsString (..))

addOne :: _ -> Int
addOne x = x + 1

both :: _ -> _ -> (Bool, Char)
both a b = (a, b)

wrapped :: Int -> Maybe _
wrapped n = Just (show n)

same :: _a -> _a -> Bool
same x y = (x :: Int) == y

total :: [_] -> Integer
total = sum

newtype Name = Name String deriving (Eq, Show)

instance IsString Name where
  fromString = Name

newtype Upper = Upper String deriving (Show)

instance IsString Upper where
  fromString = Upper . map up
    where
      up c = if c >= 'a' && c <= 'z' then toEnum (fromEnum c - 32) else c

greet :: Name -> String
greet "world" = "hello, world"
greet (Name n) = "hi " ++ n

kind :: String -> Int
kind "one" = 1
kind "two" = 2
kind _ = 0

main :: IO ()
main = do
  print (addOne 1, both True 'c', wrapped 5, same 1 1, total [1, 2, 3])
  print ("ann" :: Name, "shout" :: Upper)
  putStrLn (greet "world")
  putStrLn (greet "bob")
  print (map kind ["one", "two", "three"])
  -- A string is still a String where one is wanted, and a list of characters where a list is.
  putStrLn ("plain" ++ " strings")
  print (length ("four" :: String), words "a b c", map fromEnum "ab")
  print (("x" :: Name) == "x", lookup "k" [("k" :: Name, 1 :: Int)])
