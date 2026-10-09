-- Haskell 2010 Report 5.2 / 5.8: exporting T without constructors gives an abstract type; clients use only the exported operations
module Main (main) where

import Stack

-- the hidden constructor names are free for reuse
data Other = Empty | Push Int Other deriving Show

main :: IO ()
main = do
  let s0 = empty :: Stack Int
      s1 = push 1 (push 2 (push 3 s0))
  print ("isEmpty", isEmpty s0, isEmpty s1)
  print ("top", top s1, top s0)
  print ("pop then top", fmap top (pop s1))
  print ("toList", toList s1)
  print ("derived Eq visible", s1 == s1, s0 == s1)
  print ("derived Show visible", s1)
  print ("Functor instance visible", toList (fmap (* 10) s1))
  print ("local constructors", Push 1 Empty)
