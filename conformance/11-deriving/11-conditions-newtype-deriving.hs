-- Haskell 2010 Report 11: derived instances for newtype declarations (the constructor is shown and read like a data constructor)
module Main (main) where

newtype Age = Age Int deriving (Eq, Ord, Show, Read, Bounded)

newtype Wrapper a = Wrapper {unwrap :: a} deriving (Eq, Ord, Show, Read, Bounded)

newtype Name = Name String deriving (Eq, Ord, Show, Read)

newtype Fix = Fix (Maybe Fix) deriving (Eq, Ord, Show, Read)

newtype Both a b = Both (Either a b) deriving (Eq, Ord, Show, Read)

main :: IO ()
main = do
  print (Age 30)
  print (Age (-1))
  print (Just (Age 5))
  print ("Age Eq/Ord", Age 3 == Age 3, Age 3 /= Age 4, Age 3 < Age 4, compare (Age 9) (Age 1), max (Age 1) (Age 2))
  print ("Age Read", read "Age 7" :: Age, read " ( Age (-7) ) " :: Age, reads "Age" :: [(Age, String)])
  print ("Age Bounded", minBound :: Age, maxBound :: Age)
  print (Wrapper 'x')
  print (Wrapper (-5 :: Int))
  print (Just (Wrapper [1, 2 :: Int]))
  print (Wrapper (Wrapper True))
  print ("Wrapper Eq/Ord", Wrapper 1 == Wrapper (1 :: Int), Wrapper "a" < Wrapper "b", min (Wrapper LT) (Wrapper GT))
  print ("Wrapper Read", read "Wrapper {unwrap = 3}" :: Wrapper Int, read "Wrapper { unwrap = \"s\" }" :: Wrapper String)
  print ("Wrapper Read nested", read "Wrapper {unwrap = Wrapper {unwrap = ()}}" :: Wrapper (Wrapper ()))
  print ("Wrapper Bounded", minBound :: Wrapper Bool, maxBound :: Wrapper (Bool, Ordering))
  print ("Wrapper field", unwrap (Wrapper 'q'), unwrap (read (show (Wrapper 2.5)) :: Wrapper Double))
  print (Name "alice", [Name "", Name "b\"c"])
  print ("Name", Name "a" < Name "b", Name "ab" > Name "a", read "Name \"x y\"" :: Name)
  print (Fix (Just (Fix (Just (Fix Nothing)))))
  print ("Fix", Fix Nothing < Fix (Just (Fix Nothing)), read "Fix (Just (Fix Nothing))" == Fix (Just (Fix Nothing)))
  print (Both (Left 1) :: Both Int Char, Both (Right 'r') :: Both Int Char)
  print ("Both", (Both (Left 1) :: Both Int Char) < Both (Right 'a'), read "Both (Left (-1))" == (Both (Left (-1)) :: Both Int Char))
  print ("showsPrec 11", showsPrec 11 (Age 1) "", showsPrec 11 (Wrapper 'c') "", showsPrec 10 (Age 1) "")
