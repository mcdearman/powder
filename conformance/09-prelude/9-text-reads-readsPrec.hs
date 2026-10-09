-- Haskell 2010 Report 9 (PreludeText): ReadS, reads and readsPrec return the parsed value with the remaining input, or []
module Main (main) where

readsInt :: ReadS Int
readsInt = reads

main :: IO ()
main = do
  print ("Int", reads "12 rest" :: [(Int, String)])
  print ("Int whole", reads "12" :: [(Int, String)])
  print ("Int leading space", reads "   7 x" :: [(Int, String)])
  print ("Int then punctuation", reads "7,8" :: [(Int, String)], reads "7)" :: [(Int, String)])
  print ("Int negative", reads "-5 x" :: [(Int, String)])
  print ("Int parenthesised", reads "(5) x" :: [(Int, String)], reads "((-5))" :: [(Int, String)])
  print ("Int failures", reads "" :: [(Int, String)], reads "abc" :: [(Int, String)], reads "   " :: [(Int, String)])
  print ("Int unbalanced", reads "(5" :: [(Int, String)], reads ")" :: [(Int, String)])
  print ("ReadS synonym", readsInt "42 is the answer")
  print ("Integer", reads "123456789012345678901234567890 x" :: [(Integer, String)])
  print ("Double", reads "3.5 x" :: [(Double, String)], reads "2 y" :: [(Double, String)], reads "1.0e3 z" :: [(Double, String)])
  print ("Bool", reads "True&" :: [(Bool, String)], reads "False False" :: [(Bool, String)])
  print ("Bool failures", reads "Truely" :: [(Bool, String)], reads "true" :: [(Bool, String)], reads "T" :: [(Bool, String)])
  print ("Ordering", reads "LT GT" :: [(Ordering, String)], reads "LTE" :: [(Ordering, String)])
  print ("Char", reads "'a'bc" :: [(Char, String)], reads "'\\n' " :: [(Char, String)], reads "a" :: [(Char, String)])
  print ("String", reads "\"ab\" tail" :: [(String, String)], reads "\"\"" :: [(String, String)], reads "\"a\\\"b\"c" :: [(String, String)])
  print ("String failures", reads "ab" :: [(String, String)], reads "\"unterminated" :: [(String, String)])
  print ("unit", reads "() x" :: [((), String)], reads "(" :: [((), String)])
  print ("list", reads "[1,2,3] tail" :: [([Int], String)], reads "[] x" :: [([Int], String)], reads " [ 1 , 2 ] " :: [([Int], String)])
  print ("list failures", reads "[1,2" :: [([Int], String)], reads "[1,,2]" :: [([Int], String)], reads "[1;2]" :: [([Int], String)])
  print ("tuple", reads "(1,True) x" :: [((Int, Bool), String)], reads "(1,True" :: [((Int, Bool), String)], reads "(1)" :: [((Int, Bool), String)])
  print ("Maybe", reads "Just 5 !" :: [(Maybe Int, String)], reads "Nothing more" :: [(Maybe Int, String)])
  print ("Maybe failures", reads "Just" :: [(Maybe Int, String)], reads "just 5" :: [(Maybe Int, String)])
  print ("Either", reads "Left 1 x" :: [(Either Int Bool, String)], reads "Right True" :: [(Either Int Bool, String)], reads "Right 1" :: [(Either Int Bool, String)])
  print ("readsPrec 10 Just", readsPrec 10 "Just 5" :: [(Maybe Int, String)])
  print ("readsPrec 11 Just needs parens", readsPrec 11 "Just 5" :: [(Maybe Int, String)], readsPrec 11 "(Just 5) x" :: [(Maybe Int, String)])
  print ("readsPrec 11 Nothing", readsPrec 11 "Nothing x" :: [(Maybe Int, String)])
  print ("readsPrec 11 atoms", readsPrec 11 "5 x" :: [(Int, String)], readsPrec 11 "[1] x" :: [([Int], String)], readsPrec 11 "'c'" :: [(Char, String)])
  print ("nested argument needs parens", reads "Just Just 1" :: [(Maybe (Maybe Int), String)], reads "Just (Just 1)" :: [(Maybe (Maybe Int), String)])
  print ("reads = readsPrec 0", (reads "8 z" :: [(Int, String)]) == readsPrec 0 "8 z")
  print ("chaining", [(a + b, r2) | (a, r1) <- reads "3 4 rest" :: [(Int, String)], (b, r2) <- reads r1])
