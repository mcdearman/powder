-- Haskell 2010 Report 11.1: "derived comparisons always traverse constructors from left to right", so
-- (1,undefined) == (2,undefined) is False: later fields are not examined once an earlier field decides
module Main (main) where

data P = P Int Int Int deriving (Eq, Ord, Show)

data S = SA Int | SB Int | SC deriving (Eq, Ord, Show)

data List a = Nil | Cons a (List a) deriving (Eq, Ord, Show)

data R = R {first :: Char, second :: Char} deriving (Eq, Ord, Show)

data I = Int :+ Int deriving (Eq, Ord, Show)

bot :: Int
bot = undefined

-- an infinite derived list
ones :: List Int
ones = Cons 1 ones

from :: Int -> List Int
from n = Cons n (from (n + 1))

main :: IO ()
main = do
  print ("tuple example from the Report", (1 :: Int, bot) == (2, bot))
  print ("== stops at first field", P 1 bot bot == P 2 bot bot, P 1 bot bot /= P 2 bot bot)
  print ("== stops at second field", P 1 2 bot == P 1 3 bot, P 1 2 bot /= P 1 3 bot)
  print ("compare stops at first field", compare (P 1 bot bot) (P 2 bot bot), compare (P 2 bot bot) (P 1 bot bot))
  print ("compare stops at second field", compare (P 1 2 bot) (P 1 3 bot), P 1 3 bot > P 1 2 bot)
  print ("operators stop at first difference", P 1 bot bot < P 2 bot bot, P 1 bot bot <= P 2 bot bot, P 1 bot bot > P 2 bot bot, P 1 bot bot >= P 2 bot bot)
  print ("different constructors: fields not examined", SA bot == SB bot, SA bot /= SB bot, SA bot == SC, SC == SB bot)
  print ("different constructors: compare", compare (SA bot) (SB bot), compare SC (SA bot), SA bot < SB bot, SB bot >= SC)
  print ("records", R 'a' undefined == R 'b' undefined, compare (R 'a' undefined) (R 'b' undefined), R {second = undefined, first = 'x'} < R {second = undefined, first = 'y'})
  print ("infix constructor", (1 :+ bot) == (2 :+ bot), compare (1 :+ bot) (2 :+ bot), (3 :+ bot) > (2 :+ bot))
  print ("recursive: different length", Cons 1 Nil == Cons 1 (Cons bot Nil), compare (Cons 1 Nil) (Cons 1 (Cons bot Nil)))
  print ("recursive: early difference", Cons 1 undefined == Cons 2 undefined, compare (Cons 1 undefined) (Cons (2 :: Int) undefined))
  print ("infinite vs finite", ones == Cons 1 (Cons 1 Nil), compare ones (Cons 1 (Cons 1 Nil)), Cons 1 Nil < ones)
  print ("infinite vs infinite", ones == from 1, compare ones (from 1), from 1 > ones, from 0 < from 1)
  print ("nested", Cons (P 1 bot bot) undefined == Cons (P 2 bot bot) undefined, Just (SA bot) == Just SC, [P 0 bot bot, undefined] < [P 1 bot bot])
  print ("max/min decided early", max (P 1 0 0) (P 2 0 0) == P 2 0 0, (\(P a _ _) -> a) (min (P 1 bot bot) (P 2 bot bot)))
