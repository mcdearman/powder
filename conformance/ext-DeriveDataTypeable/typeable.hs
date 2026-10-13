{-# LANGUAGE GHC2021 #-}
-- Typeable: what a type is, asked for when a program runs. Every type is one that can be asked for, and deriving
-- that it is says nothing
module Main (main) where

import Data.Kind (Type)
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy (..))
import Data.Type.Equality ((:~:) (..))
import Data.Typeable (Typeable, cast, eqT, gcast, tyConName, typeOf, typeRep, typeRepArgs, typeRepTyCon)

data Colour = Red | Green deriving (Show, Eq, Typeable)

data Pair a b = Pair a b deriving (Show)

newtype Wrap (f :: Type -> Type) a = Wrap (f a)

data Tree a = Leaf | Node (Tree a) a (Tree a)

-- A value of any type that can be asked for, and shown.
data Dynamic = forall a. (Typeable a, Show a) => Dynamic a

fromDynamic :: Typeable b => Dynamic -> Maybe b
fromDynamic (Dynamic x) = cast x

describe :: Dynamic -> String
describe (Dynamic x) = show x ++ " :: " ++ show (typeOf x)

-- One thing for a number and another for anything else.
double :: Typeable a => a -> a
double x = case cast x of
  Just n -> case cast (n * 2 :: Int) of
    Just y -> y
    Nothing -> x
  Nothing -> x

-- The same, by knowing the two types are one.
increment :: forall a. Typeable a => a -> a
increment x = case eqT :: Maybe (a :~: Int) of
  Just Refl -> x + 1
  Nothing -> x

same :: forall a b. (Typeable a, Typeable b) => a -> b -> Bool
same _ _ = case eqT :: Maybe (a :~: b) of
  Just Refl -> True
  Nothing -> False

main :: IO ()
main = do
  print (typeOf (1 :: Int), typeOf 'c', typeOf True, typeOf (2.5 :: Double), typeOf (3 :: Integer), typeOf ())
  print (typeOf "string", typeOf [Just True], typeOf (1 :: Int, 'x'), typeOf (1 :: Int, 'x', [()]))
  print (typeOf (Just (Left 1 :: Either Int (Maybe Bool))))
  print (typeOf not, typeOf (compare :: Int -> Int -> Ordering), typeOf (map :: (Int -> Bool) -> [Int] -> [Bool]))
  print (typeOf Red, typeOf (Pair Red 'x'), typeOf (Wrap (Just Red)), typeOf (Node Leaf [Red] Leaf))
  print (typeOf (Just (id :: Int -> Int)), typeOf [Pair 'a' (Just (1 :: Int))])
  print (typeRep (Proxy :: Proxy Maybe), typeRep (Proxy :: Proxy Either), typeRep (Proxy :: Proxy (Either Int)), typeRep (Proxy :: Proxy []))
  print (typeRep (Proxy :: Proxy Pair), typeRep (Proxy :: Proxy (Wrap Maybe)), typeRep (Proxy :: Proxy (,)), typeRep (Proxy :: Proxy ((,) Int)))
  print (typeOf (1 :: Int) == typeOf (2 :: Int), typeOf (1 :: Int) == typeOf 'c', typeOf [Red] == typeOf [Green], typeOf (Just Red) == typeOf (Just 'r'))
  print (tyConName (typeRepTyCon (typeOf (Pair Red Red))), typeRepArgs (typeOf (Pair Red 'x')))
  print (cast (1 :: Int) :: Maybe Int, cast (1 :: Int) :: Maybe Integer, cast "abc" :: Maybe String, cast Red :: Maybe Colour, cast 'r' :: Maybe Colour)
  print (gcast (Just (1 :: Int)) :: Maybe (Maybe Int), gcast (Just 'c') :: Maybe (Maybe Int), gcast [True] :: Maybe [Bool])
  let things = [Dynamic (1 :: Int), Dynamic "two", Dynamic Red, Dynamic (4 :: Int), Dynamic [Just 'x'], Dynamic (Pair Green (5 :: Int))]
  mapM_ (putStrLn . describe) things
  print (mapMaybe fromDynamic things :: [Int])
  print (mapMaybe fromDynamic things :: [String], mapMaybe fromDynamic things :: [Colour])
  print (double (21 :: Int), double 'x', double "unchanged", double (1.5 :: Double))
  print (increment (41 :: Int), increment 'x', increment (41 :: Integer))
  print (same 'a' 'b', same 'a' "b", same (Just Red) (Just Green), same (Just Red) [Red])
