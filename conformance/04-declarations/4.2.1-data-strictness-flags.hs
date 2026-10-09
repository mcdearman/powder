-- Haskell 2010 Report 4.2.1: strictness flags on constructor fields; lazy fields may hold unevaluated bottoms
module Main (main) where

infixr 5 :!:

data SP = SP !Int !Int

data Mixed a = Mixed !a a

data SList = SNil | SCons !Int !SList

data Lazy = Lazy Int Int

data SRec = SRec { sx :: !Int, sy :: Int }

data STree a = STip | SBin !(STree a) !a !(STree a)

data SPair a b = !a :!: !b

sumSP :: SP -> Int
sumSP (SP a b) = a + b

mixedFirst :: Mixed a -> a
mixedFirst (Mixed x _) = x

sumSList :: SList -> Int
sumSList SNil = 0
sumSList (SCons n rest) = n + sumSList rest

fromInts :: [Int] -> SList
fromInts = foldr SCons SNil

lazySecond :: Lazy -> Int
lazySecond (Lazy _ y) = y

insertS :: Ord a => a -> STree a -> STree a
insertS x STip = SBin STip x STip
insertS x t@(SBin l v r)
  | x < v = SBin (insertS x l) v r
  | x > v = SBin l v (insertS x r)
  | otherwise = t

toListS :: STree a -> [a]
toListS STip = []
toListS (SBin l v r) = toListS l ++ [v] ++ toListS r

unSPair :: SPair a b -> (a, b)
unSPair (a :!: b) = (a, b)

main :: IO ()
main = do
  print ("strict pair", sumSP (SP 3 4))
  print ("strict constructor as function", map (sumSP . SP 10) [1, 2, 3])
  print ("strict constructor zipWith", map sumSP (zipWith SP [1, 2] [10, 20]))
  print ("mixed", mixedFirst (Mixed 'a' 'b'))
  print ("mixed lazy second field unevaluated", mixedFirst (Mixed (1 :: Int) undefined))
  print ("lazy field unevaluated", lazySecond (Lazy undefined 2))
  print ("strict list", sumSList (fromInts [1 .. 10]))
  print ("strict list empty", sumSList SNil)
  print ("strict record", sx (SRec 1 2), sy (SRec 1 2))
  print ("strict record labels", sx (SRec { sy = 5, sx = 6 }))
  print ("strict record lazy field unevaluated", sx (SRec { sx = 7, sy = undefined }))
  print ("strict record update", sy ((SRec 1 2) { sx = 9 }))
  print ("strict tree", toListS (foldr insertS STip [3, 1, 2, 3, 0 :: Int]))
  print ("strict infix", unSPair ('a' :!: True))
  print ("strict infix nested", unSPair (snd (unSPair ((1 :: Int) :!: 'b' :!: "c"))))
