-- Haskell 2010 Report 4.3.1, 6.3.1: an Eq instance giving only (==) gets (/=) from the class default, and
-- one giving only (/=) gets (==)
module Main (main) where

data Switch = Off | On

instance Eq Switch where
  Off == Off = True
  On == On = True
  _ == _ = False

data Parity = Even | Odd

instance Eq Parity where
  Even /= Odd = True
  Odd /= Even = True
  _ /= _ = False

main :: IO ()
main = do
  print ("defined ==", Off == Off, Off == On, On == On)
  print ("default /=", Off /= On, On /= On, Off /= Off)
  print ("defined /=", Even /= Odd, Even /= Even)
  print ("default ==", Even == Even, Odd == Even, Odd == Odd)
  print ("in a list", [Off, On] == [Off, On], [Off, On] /= [On, Off], elem On [Off, Off])
  print ("lookup uses ==", lookup Odd [(Even, "even"), (Odd, "odd")])
