-- Haskell 2010 Report 4.3.1: operators as class methods and fixity declarations inside class bodies
module Main (main) where

class Semi a where
  infixr 6 <+>
  (<+>) :: a -> a -> a
  infixl 7 `times`
  times :: Int -> a -> a
  times n x = if n <= 1 then x else x <+> times (n - 1) x

class Vec a where
  (^+^), (^-^) :: a -> a -> a
  zero :: a
  neg :: a -> a
  a ^-^ b = a ^+^ neg b
  infixl 6 ^+^, ^-^

newtype Sub = Sub Int deriving (Eq, Show)

newtype MaxI = MaxI Int deriving (Eq, Show)

data V = V Int Int deriving (Eq, Show)

-- infix definition in the instance
instance Semi [b] where
  xs <+> ys = xs ++ ys

-- deliberately non-associative so the declared associativity is visible
instance Semi Sub where
  Sub a <+> Sub b = Sub (a - b)

-- prefix definition in the instance
instance Semi MaxI where
  (<+>) (MaxI a) (MaxI b) = MaxI (max a b)
  times _ x = x

instance Vec V where
  V a b ^+^ V c d = V (a + c) (b + d)
  zero = V 0 0
  neg (V a b) = V (negate a) (negate b)

main :: IO ()
main = do
  print ("list", "a" <+> "b" <+> "c")
  print ("infixr 6", Sub 10 <+> Sub 3 <+> Sub 2)
  print ("explicit left", (Sub 10 <+> Sub 3) <+> Sub 2)
  print ("looser than times", "c" <+> 2 `times` "ab")
  print ("times tighter on both sides", 2 `times` "x" <+> 2 `times` "y")
  print ("tighter than ==", "a" <+> "b" == "ab")
  print ("tighter than cons", ("a" <+> "b" : []))
  print ("prefix method", (<+>) (MaxI 3) (MaxI 9))
  print ("times default", 3 `times` Sub 1)
  print ("times overridden", 5 `times` MaxI 2)
  print ("section", map (<+> "!") ["a", "b"])
  print ("vec add", V 1 2 ^+^ V 3 4)
  print ("vec default sub", V 10 10 ^-^ V 3 3 ^-^ V 2 2)
  print ("vec mixed infixl 6", V 1 2 ^+^ V 3 4 ^-^ V 1 1 ^+^ zero)
  print ("vec compare", V 1 1 ^+^ V 1 1 == V 2 2)
