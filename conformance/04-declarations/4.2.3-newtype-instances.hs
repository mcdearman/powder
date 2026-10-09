-- Haskell 2010 Report 4.2.3: a newtype is a distinct type and can carry its own class instances
module Main (main) where

newtype Mod7 = Mod7 Int deriving (Eq, Show)

instance Num Mod7 where
  Mod7 a + Mod7 b = Mod7 ((a + b) `mod` 7)
  Mod7 a * Mod7 b = Mod7 ((a * b) `mod` 7)
  negate (Mod7 a) = Mod7 ((7 - a) `mod` 7)
  abs x = x
  signum (Mod7 a) = Mod7 (signum a)
  fromInteger n = Mod7 (fromInteger (n `mod` 7))

-- same representation as Int, opposite ordering
newtype Rev a = Rev a deriving (Eq, Show)

instance Ord a => Ord (Rev a) where
  compare (Rev x) (Rev y) = compare y x

newtype Pred a = Pred (a -> Bool)

runPred :: Pred a -> a -> Bool
runPred (Pred p) = p

andPred :: Pred a -> Pred a -> Pred a
andPred (Pred p) (Pred q) = Pred (\x -> p x && q x)

newtype Box f a = Box (f a)

instance Functor f => Functor (Box f) where
  fmap g (Box x) = Box (fmap g x)

unBox :: Box f a -> f a
unBox (Box x) = x

-- different Show from the underlying type
newtype Hex = Hex Int

instance Show Hex where
  show (Hex n) = "0x" ++ digits n
    where
      digits k
        | k < 16 = [digit k]
        | otherwise = digits (k `div` 16) ++ [digit (k `mod` 16)]
      digit d = "0123456789abcdef" !! d

main :: IO ()
main = do
  print ("mod7 add", Mod7 5 + Mod7 4)
  print ("mod7 mul literal", 3 * 5 :: Mod7)
  print ("mod7 sub", Mod7 3 - Mod7 5)
  print ("mod7 pow", (2 :: Mod7) ^ (10 :: Int))
  print ("mod7 sum", sum [1, 2, 3, 4 :: Mod7])
  print ("mod7 literal wraps", 100 :: Mod7)
  print ("rev compare", compare (Rev 1) (Rev (2 :: Int)), Rev 'a' < Rev 'b')
  print ("rev maximum", maximum [Rev 3, Rev 1, Rev (2 :: Int)])
  print ("rev min", min (Rev "a") (Rev "b"))
  print ("pred", runPred (Pred even) (4 :: Int))
  print ("pred and", map (runPred (andPred (Pred even) (Pred (> 2)))) [1, 2, 3, 4 :: Int])
  print ("box maybe", unBox (fmap (+ 1) (Box (Just (1 :: Int)))))
  print ("box list", unBox (fmap show (Box [1, 2 :: Int])))
  print ("hex", Hex 255, Hex 0, Hex 4096)
  print ("hex list", [Hex 10, Hex 17])
