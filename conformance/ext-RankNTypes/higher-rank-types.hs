{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE RankNTypes #-}
-- RankNTypes: a function given something that works at every type, which it then uses at more than one
module Main (main) where

import Data.Char (toUpper)

-- The argument is used at two types.
both :: (forall a. a -> a) -> (Int, Bool) -> (Int, Bool)
both f (n, b) = (f n, f b)

-- With a class asked of the type it works at.
showBoth :: (forall a. Show a => a -> String) -> Int -> Bool -> String
showBoth sh n b = sh n ++ " " ++ sh b

-- A list function used on lists of two kinds.
onLists :: (forall a. [a] -> [a]) -> ([Int], String) -> ([Int], String)
onLists f (xs, s) = (f xs, f s)

-- Inside a data type: numbers as Church has them.
newtype Church = Church (forall a. (a -> a) -> a -> a)

zero :: Church
zero = Church (\_ z -> z)

suc :: Church -> Church
suc (Church n) = Church (\s z -> s (n s z))

add :: Church -> Church -> Church
add (Church m) (Church n) = Church (\s z -> m s (n s z))

toInt :: Church -> Int
toInt (Church n) = n (+ 1) 0

toStr :: Church -> String
toStr (Church n) = n ('*' :) ""

-- A record of functions that each work at every type.
data Monoidal f = Monoidal
  { unit :: forall a. f a,
    plus :: forall a. f a -> f a -> f a
  }

listMonoidal :: Monoidal []
listMonoidal = Monoidal {unit = [], plus = (++)}

maybeMonoidal :: Monoidal Maybe
maybeMonoidal = Monoidal Nothing (\a b -> case a of Nothing -> b; _ -> a)

joined :: Monoidal f -> [f a] -> f a
joined m = foldr (plus m) (unit m)

-- Given on to another that wants the same.
twice :: (forall a. a -> a) -> (Int, Bool) -> (Int, Bool)
twice f = both f . both f

-- A continuation that must work whatever the answer's type.
newtype Cont a = Cont (forall r. (a -> r) -> r)

runCont :: Cont a -> (a -> r) -> r
runCont (Cont k) = k

pureC :: a -> Cont a
pureC a = Cont (\k -> k a)

bindC :: Cont a -> (a -> Cont b) -> Cont b
bindC (Cont m) f = Cont (\k -> m (\a -> runCont (f a) k))

-- With a class of type constructors.
mapBoth :: Functor f => (forall a. a -> f a) -> Int -> Char -> (f Int, f Char)
mapBoth wrap n c = (wrap n, wrap c)

-- Something used at two types is given something of that kind, by name or written out.
main :: IO ()
main = do
  print (both id (1, True), both (\x -> x) (2, False))
  putStrLn (showBoth show 1 True)
  putStrLn (showBoth (\x -> "<" ++ show x ++ ">") 2 False)
  print (onLists reverse ([1, 2, 3], "abc"), onLists (take 2) ([1, 2, 3], "abc"))
  let three = suc (suc (suc zero))
  print (toInt three, toInt (add three three), toStr three)
  print (joined listMonoidal [[1, 2], [3], [4 :: Int]], joined maybeMonoidal [Nothing, Just 'a', Just 'b'])
  print (plus listMonoidal "ab" "cd", (unit maybeMonoidal :: Maybe Int))
  print (twice id (5, True))
  print (runCont (pureC 3 `bindC` \x -> pureC (x * 2 :: Int)) show, runCont (pureC 'a') toUpper)
  print (mapBoth Just 1 'c', mapBoth (: []) 2 'd')
