-- Haskell 2010 Report 11.4: derived readsPrec d: a prefix application is read without parentheses only when d <= 10,
-- an infix constructor of precedence p only when d <= p; with parentheses any d works; nullary constructors always
module Main (main) where

infixr 5 :+:

infixl 6 :-:

data T = N | U Int | T :+: T | T :-: T deriving (Eq, Show, Read)

-- the complete parses of a string at each precedence from 0 to 11, as a string of Y/n flags
flags :: String -> String
flags s = map (\d -> if null [() | (_, "") <- readsPrec d s :: [(T, String)]] then 'n' else 'Y') [0 .. 11]

row :: String -> IO ()
row s = putStrLn (flags s ++ "  " ++ s)

main :: IO ()
main = do
  putStrLn "012345678901  (precedence 0..11)"
  row "N"
  row "(N)"
  row "U 1"
  row "(U 1)"
  row "((U 1))"
  row "U (-1)"
  row "N :+: N"
  row "(N :+: N)"
  row "N :-: N"
  row "(N :-: N)"
  row "U 1 :+: U 2"
  row "U 1 :-: U 2"
  row "N :-: N :+: N"
  row "N :+: N :-: N"
  row "(N :+: N) :-: N"
  row "N :+: (N :+: N)"
  row "N :+: N :+: N"
  row "N :-: N :-: N"
  print ("values at precedence 0", [x | (x, "") <- readsPrec 0 "N :-: N :+: N" :: [(T, String)]])
  print ("values at precedence 5", [x | (x, "") <- readsPrec 5 "U 1 :+: U 2" :: [(T, String)]])
  print ("values at precedence 6", [x | (x, "") <- readsPrec 6 "U 1 :-: U 2" :: [(T, String)]])
  print ("values at precedence 10", [x | (x, "") <- readsPrec 10 "U 1" :: [(T, String)]])
  print ("values at precedence 11", [x | (x, "") <- readsPrec 11 "(U 1 :+: N)" :: [(T, String)]])
  print ("show then read at every precedence", and [[x | (x, "") <- readsPrec d (showsPrec d v "")] == [v] | d <- [0 .. 11], v <- [N, U 1, U (-1), N :+: N, U 1 :-: U 2, (N :+: N) :-: N, N :+: (U 2 :+: N)]])
  print ("Prelude types follow the same rule", null (readsPrec 11 "Just 1" :: [(Maybe Int, String)]), readsPrec 10 "Just 1" :: [(Maybe Int, String)], readsPrec 11 "(Just 1)" :: [(Maybe Int, String)])
  print ("Either", null (readsPrec 11 "Left 1" :: [(Either Int Int, String)]), readsPrec 11 "(Right 2)" :: [(Either Int Int, String)], readsPrec 11 "Nothing" :: [(Maybe Int, String)])
