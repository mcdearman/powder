{-# LANGUAGE StrictData #-}
-- StrictData: a constructor's fields are strict unless marked lazy with `~`
module Main (main) where

data Pair = Pair Int Int

data Lazy = Lazy ~Int Int

data Rec = Rec {strictField :: Int, lazyField :: ~Int}

newtype Wrap = Wrap Int

data Poly a = Poly a ~a

data Op a b = a :+ ~b

second :: Pair -> Int
second (Pair _ b) = b

main :: IO ()
main = do
  print (second (Pair 1 2))
  print (case Lazy undefined 2 of Lazy _ b -> b)
  print (strictField (Rec 3 undefined))
  print (case Rec {strictField = 4, lazyField = undefined} of Rec {strictField = s} -> s)
  -- A newtype's field is never strict: the constructor is not there.
  print (case Wrap undefined of Wrap _ -> "newtype")
  print (case Poly 'a' undefined of Poly c _ -> c)
  print (case 'l' :+ undefined of c :+ _ -> c)
  -- Putting something undefined in a strict field is undefined as soon as the whole is looked at.
  Pair 1 undefined `seq` putStrLn "not reached"
