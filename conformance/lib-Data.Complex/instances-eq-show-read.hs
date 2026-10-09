-- Haskell 2010 Report 17 (Data.Complex): Eq, Show and Read instances (derived form, constructor precedence 6)
module Main (main) where

import Data.Complex

type C = Complex Double

main :: IO ()
main = do
  print (1 :+ 2 :: C)
  print (1 :+ (-2) :: C)
  print ((-1) :+ 2 :: C)
  print ((-1.5) :+ (-2.5) :: C)
  print (0 :+ 0 :: C)
  print (1.0e-2 :+ 1.0e7 :: C)
  print (Just (1 :+ 2 :: C))
  print [1 :+ 2, 3 :+ 4 :: C]
  print (1 :+ 2 :: C, 'x')
  print (1.5 :+ 2.5 :: Complex Float)
  putStrLn (showsPrec 6 (1 :+ 2 :: C) "")
  putStrLn (showsPrec 7 (1 :+ 2 :: C) "")
  putStrLn (showsPrec 0 (1 :+ 2 :: C) "<rest")
  print ("eq", (1 :+ 2 :: C) == 1 :+ 2)
  print ("eq real differs", (1 :+ 2 :: C) == 9 :+ 2)
  print ("eq imag differs", (1 :+ 2 :: C) == 1 :+ 9)
  print ("neq", (1 :+ 2 :: C) /= 1 :+ 3)
  print ("eq with literal", (3 :+ 0 :: C) == 3)
  print ("read", (read "1.0 :+ 2.0" :: C))
  print ("read negative imaginary", (read "1.0 :+ (-2.0)" :: C))
  print ("read negative real", (read "(-1.0) :+ 2.0" :: C))
  print ("read parenthesised", (read " ( 3.5 :+ 4.5 ) " :: C))
  print ("read integer syntax parts", (read "3 :+ 4" :: C))
  print ("read in Just", (read "Just (1.0 :+ 2.0)" :: Maybe C))
  print ("read list", (read "[1.0 :+ 2.0,3.0 :+ 4.0]" :: [C]))
  print ("read . show", (read (show ((-1.25) :+ 8 :: C)) :: C) == (-1.25) :+ 8)
  print ("reads rest", (reads "1.0 :+ 2.0 tail" :: [(C, String)]))
  print ("readsPrec 7 needs parentheses", null (readsPrec 7 "1.0 :+ 2.0" :: [(C, String)]))
  print ("readsPrec 7 parenthesised", (readsPrec 7 "(1.0 :+ 2.0)" :: [(C, String)]))
