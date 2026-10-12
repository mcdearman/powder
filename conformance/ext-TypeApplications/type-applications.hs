{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE TypeApplications, ScopedTypeVariables, AllowAmbiguousTypes #-}
-- TypeApplications: a function applied to a type, after an @, which is what the first of its type variables is
module Main (main) where

import Data.List (sort)

pair :: forall a b. a -> b -> (a, b)
pair x y = (x, y)

convert :: forall a b. (Show a, Read b) => a -> b
convert = read . show

class Named a where
  name :: String

instance Named Int where
  name = "Int"

instance Named Bool where
  name = "Bool"

instance Named a => Named [a] where
  name = "[" ++ name @a ++ "]"

sizeOf :: forall a. (Bounded a, Enum a) => Int
sizeOf = fromEnum (maxBound @a) - fromEnum (minBound @a) + 1

-- In the order the signature mentions them, where there is no forall.
swapped :: (a, b) -> (b, a)
swapped (x, y) = (y, x)

-- A type left out with _ leaves its variable to be found.
main :: IO ()
main = do
  print (read @Int "42", read @Double "1.5", read @[Int] "[1,2,3]")
  print (show @Integer 7, fromIntegral @Int @Double 3)
  print (maxBound @Int > 0, minBound @Bool, maxBound @Char > 'z')
  print (pair @Int @Char 1 'c', pair @_ @Bool 'x' True)
  print (convert @Int @Double 3, convert @_ @Int (12 :: Integer))
  putStrLn (name @Int ++ " " ++ name @Bool ++ " " ++ name @[[Int]])
  print (sizeOf @Bool, sizeOf @Ordering)
  print (Just @Int 5, Left @String @Int "l", Nothing @Char)
  print (swapped @Int @Char (1, 'x'))
  print (map (fromIntegral @Int @Integer) [1, 2, 3], sort @Int [3, 1, 2])
  print (mempty @String, pure @Maybe 'p', fmap @[] (+ 1) [1, 2 :: Int])
  print (foldr @[] (+) 0 [1, 2, 3 :: Int], traverse @[] @Maybe Just "ab")
