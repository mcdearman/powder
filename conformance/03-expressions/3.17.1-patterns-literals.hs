-- Haskell 2010 Report 3.17.1: literal patterns (integer, negative, float, char, string)
module Main (main) where

num :: Int -> String
num 0 = "zero"
num 1 = "one"
num (-1) = "minus one"
num 0x10 = "sixteen"
num 0o10 = "eight"
num _ = "many"

big :: Integer -> Bool
big 12345678901234567890 = True
big _ = False

dbl :: Double -> String
dbl 0.5 = "half"
dbl 1 = "one"
dbl (-2.5) = "minus two and a half"
dbl 1e2 = "hundred"
dbl _ = "other"

rat :: Rational -> String
rat 0.75 = "three quarters"
rat 2 = "two"
rat _ = "other"

chr1 :: Char -> Int
chr1 'a' = 1
chr1 '\n' = 2
chr1 '\'' = 3
chr1 '\x41' = 4
chr1 '\\' = 5
chr1 ' ' = 6
chr1 _ = 0

str :: String -> Int
str "" = 0
str "hello" = 1
str "a\tb" = 2
str "\"" = 3
str ('x' : _) = 4
str _ = 5

main :: IO ()
main = do
  print ("int", map num [0, 1, -1, 16, 8, 2])
  print ("integer", big 12345678901234567890, big 12345678901234567891)
  print ("double", map dbl [0.5, 1.0, -2.5, 100.0, 3.0])
  print ("rational", map rat [0.75, 2, 1.5])
  print ("char", map chr1 "a\n'A\\ z")
  print ("string", map str ["", "hello", "a\tb", "\"", "xyz", "hell", "hello!"])
  print ("negative in case", [case n of { -1 -> "m1"; -2 -> "m2"; _ -> "other" } | n <- [-1, -2, 1]])
  print ("negative float in case", case -0.5 of { -0.5 -> "neg half"; _ -> "other" })
  print ("literal in tuple", case (0, 'a', "s") of { (0, 'a', "s") -> "all three"; _ -> "no" })
  print ("literal in list", case [1, 2, 3] of { [1, _, 3] -> "ends"; _ -> "no" })
  print ("literal in lambda", (\0 'c' -> "zero c") 0 'c')
  print ("string is a list pattern", case "ab" of { ['a', 'b'] -> "chars"; _ -> "no" }, case ['a', 'b'] of { "ab" -> "string"; _ -> "no" })
  print ("equal literals different spelling", case 255 of { 0xff -> "hex"; _ -> "no" }, case 1.0 of { 1 -> "int literal"; _ -> "no" })
  print ("char escapes", case '\65' of { 'A' -> "decimal escape"; _ -> "no" }, case '\t' of { '\HT' -> "ascii name"; _ -> "no" })
