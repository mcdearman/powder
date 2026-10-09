-- Haskell 2010 Report 4.5.2: let-bound variables are generalised and may be used at several types
module Main (main) where

-- g is generalised over the type of y only; x stays the (monomorphic) argument of f
capture :: a -> ((a, Char), (a, Bool))
capture x =
  let g y = (x, y)
  in (g 'a', g True)

whereBound :: (Int, String, [Bool])
whereBound = (len [1, 2, 3 :: Int], rev "abc", rev [True, False])
  where
    len xs = foldr (\_ n -> n + 1) 0 xs
    rev xs = foldl (flip (:)) [] xs

nestedLets :: ((Int, Bool), (Char, String))
nestedLets =
  let pair x y = (x, y)
  in let swap (x, y) = (y, x)
         both = pair
     in (swap (both True 1), swap (pair "s" 'c'))

main :: IO ()
main = do
  print ("identity", let i x = x in (i (1 :: Int), i 'c', i "s"))
  print ("self application of let-bound", let i x = x in i i True)
  print ("pair", let dup x = (x, x) in (dup 'a', dup False))
  print ("const", let k x _ = x in (k 'a' True, k True 'a'))
  print ("compose", let o f g x = f (g x) in (o not not True, o length show (12345 :: Int)))
  print ("overloaded function", let sq x = x * x in (sq (3 :: Int), sq (1.5 :: Double)))
  print ("overloaded show", let sh x = show x ++ "." in (sh True, sh 'c', sh [1 :: Int]))
  print ("capture", capture (0 :: Int))
  print ("where", whereBound)
  print ("nested", nestedLets)
  let twice f = f . f
      empty = []
  print ("do-let function", twice (+ 1) (0 :: Int), twice ('x' :) "")
  print ("do-let value", length (True : empty), length ('c' : empty))
  print ("polymorphic data", let none = Nothing in (none == Just 'a', none == Just True))
