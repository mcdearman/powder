{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
-- DataKinds: a number or a string is a type, and a class says which one a type is
module Main (main) where

import Data.Proxy (Proxy (..))
import GHC.TypeLits (KnownNat, KnownSymbol, Nat, Symbol, natVal, symbolVal)

-- A number that is in the type and not in the value.
newtype Mod (n :: Nat) = Mod Integer

modulus :: KnownNat n => Mod n -> Integer
modulus = natVal

mk :: KnownNat n => Integer -> Mod n
mk x = result
  where
    result = Mod (x `mod` modulus result)

plus :: KnownNat n => Mod n -> Mod n -> Mod n
plus (Mod a) (Mod b) = mk (a + b)

times :: KnownNat n => Mod n -> Mod n -> Mod n
times (Mod a) (Mod b) = mk (a * b)

value :: Mod n -> Integer
value (Mod a) = a

-- A field with its name in its type.
newtype Field a (name :: Symbol) = Field a

label :: KnownSymbol name => Field a name -> String
label = symbolVal

describe :: (KnownSymbol name, Show a) => Field a name -> String
describe f@(Field x) = label f ++ " = " ++ show x

age :: Field Int "age"
age = Field 41

called :: Field String "name"
called = Field "Ada"

-- One of each in a type, and both asked for.
data Sized (n :: Nat) (unit :: Symbol) = Sized

measure :: (KnownNat n, KnownSymbol unit) => Sized n unit -> String
measure s = show (natVal (size s)) ++ " " ++ symbolVal (unit s)
  where
    size :: Sized n unit -> Proxy n
    size _ = Proxy
    unit :: Sized n unit -> Proxy unit
    unit _ = Proxy

main :: IO ()
main = do
  print (natVal (Proxy :: Proxy 0), natVal (Proxy :: Proxy 42), natVal (Proxy :: Proxy 18446744073709551616))
  putStrLn (symbolVal (Proxy :: Proxy "a string"))
  print (symbolVal (Proxy :: Proxy ""), symbolVal (Proxy :: Proxy "with \"quotes\" and a\ttab"))
  let a = mk 5 :: Mod 7
      b = mk 4
  print (value (plus a b), value (times a b), modulus a)
  print (value (times (mk 123456789 :: Mod 1000) (mk 987654321)))
  putStrLn (describe age)
  putStrLn (describe called)
  putStrLn (measure (Sized :: Sized 3 "metres"))
