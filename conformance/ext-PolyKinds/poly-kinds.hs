{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
-- PolyKinds: a type whose parameter is of any kind, a different one at each use
module Main (main) where

import Data.Kind (Type)

data Proxy (a :: k) = Proxy

data Colour = Red | Green

-- At a type, at a type that takes a type, and at a constructor as a type.
atType :: Proxy Int
atType = Proxy

atFunction :: Proxy Maybe
atFunction = Proxy

atConstructor :: Proxy 'Red
atConstructor = Proxy

class Named (a :: k) where
  name :: Proxy a -> String

instance Named Int where
  name _ = "Int"

instance Named Maybe where
  name _ = "Maybe"

instance Named 'Red where
  name _ = "Red"

instance Named 'Green where
  name _ = "Green"

instance Named Either where
  name _ = "Either"

-- One that takes a type of any kind and a function from that kind.
newtype App (f :: k -> Type) (a :: k) = App (f a)

unApp :: App f a -> f a
unApp (App x) = x

data Swatch (c :: Colour) = Swatch Int

paint :: Proxy c -> Int -> App Swatch c
paint _ n = App (Swatch n)

depth :: Swatch c -> Int
depth (Swatch n) = n

same :: Proxy a -> Proxy a -> Bool
same _ _ = True

main :: IO ()
main = do
  putStrLn (unwords [name atType, name atFunction, name atConstructor, name (Proxy :: Proxy 'Green), name (Proxy :: Proxy Either)])
  print (unApp (App (Just 'x')), unApp (App [1, 2, 3 :: Int]))
  print (depth (unApp (paint atConstructor 7)))
  print (same atFunction Proxy, same atConstructor Proxy)
