{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DefaultSignatures, DeriveAnyClass, DerivingStrategies #-}
-- DeriveGeneric: a type's values as values of the few types every type's can be kept as, with the names of the
-- type, its constructors and their fields; so that one function shows, counts or fills in any of them
module Main (main) where

import Data.List (intercalate)
import Data.Proxy (Proxy (..))
import GHC.Generics

data Colour = Red | Green | Blue deriving (Show, Eq, Generic)

data Shape = Circle Double | Rect Double Double | Dot deriving (Show, Generic)

data Person = Person {name :: String, age :: Int, likes :: [Colour]} deriving (Show, Generic)

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show, Generic)

data Pair a b = a :& b deriving (Show, Generic)

infixr 4 :&

newtype Wrapped = Wrapped {unwrap :: Int} deriving (Show, Generic)

data Five = One | Two Int | Three Int Bool | Four Int Bool Char | Five' Int Bool Char () deriving (Show, Generic)

-- Shown, by what its type is kept as.
class GShow f where
  gshow :: f p -> [String]

instance GShow U1 where
  gshow _ = []

instance Show c => GShow (K1 i c) where
  gshow (K1 x) = [showsPrec 11 x ""]

instance (GShow f, GShow g) => GShow (f :*: g) where
  gshow (a :*: b) = gshow a ++ gshow b

instance (GShow f, GShow g) => GShow (f :+: g) where
  gshow (L1 a) = gshow a
  gshow (R1 b) = gshow b

instance GShow f => GShow (D1 c f) where
  gshow (M1 x) = gshow x

instance (GShow f, Constructor c) => GShow (C1 c f) where
  gshow m@(M1 x) = [unwords (conName m : gshow x)]

instance (GShow f, Selector s) => GShow (S1 s f) where
  gshow m@(M1 x) = case selName m of
    "" -> gshow x
    field -> [field ++ "=" ++ concat (gshow x)]

genericShow :: (Generic a, GShow (Rep a)) => a -> String
genericShow = concat . gshow . from

-- Which constructor, counted from none, and of how many.
class GIndex f where
  gindex :: f p -> Int
  gcount :: Proxy f -> Int

instance GIndex (C1 c f) where
  gindex _ = 0
  gcount _ = 1

instance (GIndex f, GIndex g) => GIndex (f :+: g) where
  gindex (L1 a) = gindex a
  gindex (R1 b) = gcount (Proxy :: Proxy f) + gindex b
  gcount _ = gcount (Proxy :: Proxy f) + gcount (Proxy :: Proxy g)

instance GIndex f => GIndex (D1 c f) where
  gindex (M1 x) = gindex x
  gcount _ = gcount (Proxy :: Proxy f)

index :: (Generic a, GIndex (Rep a)) => a -> Int
index = gindex . from

-- A value with every field the least it can be: the first constructor's.
class GDefault f where
  gdefault :: f p

instance GDefault U1 where
  gdefault = U1

instance Default c => GDefault (K1 i c) where
  gdefault = K1 def

instance (GDefault f, GDefault g) => GDefault (f :*: g) where
  gdefault = gdefault :*: gdefault

instance GDefault f => GDefault (f :+: g) where
  gdefault = L1 gdefault

instance GDefault f => GDefault (M1 i c f) where
  gdefault = M1 gdefault

class Default a where
  def :: a
  default def :: (Generic a, GDefault (Rep a)) => a
  def = to gdefault

instance Default Int where def = 0

instance Default Double where def = 0

instance Default Bool where def = False

instance Default Char where def = 'a'

instance Default [a] where def = []

instance Default ()

deriving anyclass instance Default Colour

deriving anyclass instance Default Shape

deriving anyclass instance Default Person

deriving anyclass instance Default Wrapped

deriving anyclass instance Default Five

instance Default a => Default (Tree a)

instance (Default a, Default b) => Default (Pair a b)

-- What is said of a type.
described :: forall a. (Generic a, Described (Rep a)) => a -> String
described x = describe (from x)

class Described f where
  describe :: f p -> String

instance (Datatype d, Fields f) => Described (D1 d f) where
  describe m@(M1 x) = intercalate " " ([moduleName m ++ "." ++ datatypeName m] ++ ["newtype" | isNewtype m] ++ fields x)

class Fields f where
  fields :: f p -> [String]

instance (Fields f, Fields g) => Fields (f :+: g) where
  fields (L1 a) = fields a
  fields (R1 b) = fields b

instance (Constructor c, Names f) => Fields (C1 c f) where
  fields m@(M1 x) = [conName m, show (conFixity m), show (conIsRecord m), show (names x)]

class Names f where
  names :: f p -> [String]

instance Names U1 where
  names _ = []

instance (Names f, Names g) => Names (f :*: g) where
  names (a :*: b) = names a ++ names b

instance Selector s => Names (S1 s f) where
  names m = [selName m]

main :: IO ()
main = do
  putStrLn (intercalate "; " (map genericShow [Red, Green, Blue]))
  putStrLn (intercalate "; " (map genericShow [Circle 1.5, Rect 2 3, Dot]))
  putStrLn (genericShow (Person "Ada" 36 [Red, Blue]))
  putStrLn (genericShow (Node (Node Leaf 'a' Leaf) 'b' Leaf))
  putStrLn (genericShow ((1 :: Int) :& "two"))
  putStrLn (genericShow (Wrapped 7))
  print (map index [Red, Green, Blue], map index [Circle 1, Rect 1 1, Dot])
  print (map index [One, Two 1, Three 1 True, Four 1 True 'c', Five' 1 True 'c' ()], index (Leaf :: Tree ()), index (Node Leaf () Leaf))
  print (def :: Colour, def :: Shape, def :: Person, def :: Wrapped, def :: Five)
  print (def :: Tree Int, def :: Pair Int (Pair Bool [Char]))
  print (to (from (Person "Bo" 2 [])) :: Person, to (from (Four 4 False 'd')) :: Five)
  print ((to (from (Node Leaf (3 :: Int) Leaf)) :: Tree Int), unwrap (to (from (Wrapped 9))))
  putStrLn (described Blue)
  putStrLn (described (Rect 1 2))
  putStrLn (described (Person "" 0 []))
  putStrLn (described ('x' :& 'y'))
  putStrLn (described (Wrapped 0))
