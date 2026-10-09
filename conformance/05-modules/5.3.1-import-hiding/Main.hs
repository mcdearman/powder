-- Haskell 2010 Report 5.3.1: `hiding` imports everything except the listed entities; hiding C hides a constructor or a type/class named C
module Main (main) where

import Zoo hiding (Emu, feed, Keeper, noise, number, (<+>))

data Bird = Emu | Keeper deriving Show

feed :: Int -> Int
feed = (* 2)

noise :: Int
noise = 0

number :: Int
number = 5

(<+>) :: Int -> Int -> Int
a <+> b = a - b

main :: IO ()
main = do
  print ("not hidden", [Cat, Dog], legs Cat, zooName)
  print ("hidden constructor replaced", Emu, Keeper)
  print ("hidden function replaced", feed 4)
  print ("hidden method replaced; other method still works", noise, loud Dog)
  print ("hidden field replaced; constructor and other field remain", number, occupant (Cage Cat 1))
  print ("hidden operator replaced (local default fixity)", 10 <+> 3 <+> 2)
  print ("positional construction still possible", Cage Dog 7)
