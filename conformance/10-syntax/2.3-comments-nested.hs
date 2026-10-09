-- Haskell 2010 Report 2.3: nested comments {- ... -} nest, may span lines, and act as whitespace
module Main (main) where

{- a simple block comment -}

{- outer {- inner -} still outer -}

{- multi
   line {- nested
   deeper {- and deeper -} -}
   comment
-}

{--}

{---}

{- a line comment marker inside: -- does not matter -}

f :: Int -> {- comment in a type -} Int
f x = x {- comment in an expression -} + 1

g :: Int{-no spaces-}->{-around-}Int
g{-here-}y = y*2

{- a string-looking thing: "-}
h :: Int
h = 3

main :: IO ()
main = do
  print ("f", f 1)
  print ("g", g 21)
  print ("h", h)
  print {- between function and arg -} ("inline", 4 {- 5 -})
  print ("comment chars in string", "{-", "-}")
  {- comment as a
     whole statement -}
  print ("after block", [1 {- one -}, 2 {- two -} {- again -}, 3])
  print ("dash brace", 10 -{-x-}3)
  -- {- a line comment containing an opener does not open a block
  print ("done", True)
