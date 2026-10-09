-- Haskell 2010 Report 20.8 (Data.List): unzip4, unzip5, unzip6, unzip7
module Main (main) where

import Data.List

q4 :: [(Int, Char, Bool, Ordering)]
q4 = [(1, 'a', True, LT), (2, 'b', False, EQ), (3, 'c', True, GT)]

q5 :: [(Int, Char, Bool, Ordering, Double)]
q5 = [(1, 'a', True, LT, 0.5), (2, 'b', False, EQ, 1.5)]

q6 :: [(Int, Char, Bool, Ordering, Double, String)]
q6 = [(1, 'a', True, LT, 0.5, "x"), (2, 'b', False, EQ, 1.5, "yy")]

q7 :: [(Int, Char, Bool, Ordering, Double, String, Integer)]
q7 = [(1, 'a', True, LT, 0.5, "x", 100), (2, 'b', False, EQ, 1.5, "yy", 200)]

main :: IO ()
main = do
  print ("unzip4", unzip4 q4)
  print ("unzip4 empty", unzip4 (take 0 q4))
  print ("unzip4 singleton", unzip4 (take 1 q4))
  print ("unzip5", unzip5 q5)
  print ("unzip5 empty", unzip5 (take 0 q5))
  print ("unzip6", unzip6 q6)
  print ("unzip6 empty", unzip6 (take 0 q6))
  print ("unzip7", unzip7 q7)
  print ("unzip7 empty", unzip7 (take 0 q7))
  print ("unzip4 then zip4", (\(a, b, c, d) -> zip4 a b c d) (unzip4 q4) == q4)
  print ("unzip5 then zip5", (\(a, b, c, d, e) -> zip5 a b c d e) (unzip5 q5) == q5)
  print ("unzip6 then zip6", (\(a, b, c, d, e, f) -> zip6 a b c d e f) (unzip6 q6) == q6)
  print ("unzip7 then zip7", (\(a, b, c, d, e, f, g) -> zip7 a b c d e f g) (unzip7 q7) == q7)
  print ("unzip4 first column", (\(a, _, _, _) -> a) (unzip4 q4))
  print ("unzip7 last column", (\(_, _, _, _, _, _, g) -> sum g) (unzip7 q7))
  print ("unzip5 column lengths", (\(a, b, c, d, e) -> [length a, length b, length c, length d, length e]) (unzip5 q5))
