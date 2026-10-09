-- Haskell 2010 Report 3.17.2, 4.4.3.2: an irrefutable pattern matches without looking; a pattern binding at
-- the top level binds lazily
module Main (main) where

t, a, b, l, e :: Char
t : a : b : l : e : [] = "table"

main :: IO ()
main = case undefined :: (Int, Int) of
  ~(_, _) -> case "ir" of
    ~(i : r : []) -> do
      putStrLn (i : r : 'r' : 'e' : 'f' : 'u' : t : a : b : l : e : [])
      print (case undefined :: [Int] of ~(_ : _) -> "not forced")
      print (let ~(p, q) = undefined :: (Int, Int) in "unused")
      print ((\ ~(p, q) -> 1 :: Int) (undefined :: (Bool, Bool)))
      print (case (1 :: Int, undefined :: Int) of ~(p, _) -> p)
