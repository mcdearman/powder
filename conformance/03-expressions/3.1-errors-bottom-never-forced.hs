-- Haskell 2010 Report 3.1: errors are bottom; a bottom that is never demanded is harmless
module Main (main) where

bottom :: a
bottom = undefined

loud :: Int
loud = error "never evaluated"

constK :: a -> b -> a
constK x _ = x

unusedWhere :: Int -> Int
unusedWhere n = n + 1
  where
    _unused = loud + n

main :: IO ()
main = do
  print ("const", const (1 :: Int) (undefined :: Bool))
  print ("constK", constK 'a' loud)
  print ("fst", fst (2 :: Int, bottom :: Int))
  print ("snd", snd (bottom :: Int, "ok"))
  print ("length", length [bottom, loud, undefined])
  print ("take", take 2 (1 : 2 : bottom) :: [Int])
  print ("head", head (5 : bottom) :: Int)
  print ("and", False && bottom)
  print ("or", True || bottom)
  print ("if", if True then 1 else loud)
  print ("let", let x = loud in 7 :: Int)
  print ("case wildcard", case loud of { _ -> 'w' })
  print ("case var", case loud of { v -> 'v' })
  print ("lazy pattern", (\ ~(a, b) -> 0 :: Int) bottom)
  print ("where", unusedWhere 3)
  print ("map length", length (map (+ loud) [1, 2, 3]))
  print ("zip", zip [1 :: Int, 2] ('a' : 'b' : bottom))
  print ("null", null (loud : bottom))
  print ("seq cons", seq (loud : bottom) "whnf")
  print ("seq tuple", seq (loud, loud) "tuple")
  print ("seq lambda", seq (\x -> loud + x) "function")
  print ("seq just", seq (Just loud) "just")
  print ("maybe", maybe 'n' (const 'j') (Just loud))
  print ("error typed", length [error "a" :: Int, error "b"])
  print ("error as function", length [error "f" :: Int -> Int])
