-- Haskell 2010 Report 3.8: tuples, the tuple constructors (,) (,,) ..., fst and snd
module Main (main) where

swapPair :: (a, b) -> (b, a)
swapPair (a, b) = (b, a)

third :: (a, b, c) -> c
third (_, _, c) = c

main :: IO ()
main = do
  print ("pair", (1, 'a'))
  print ("(,)", (,) 1 'a', (,) 1 'a' == (1, 'a'))
  print ("(,,)", (,,) 1 'b' "c")
  print ("(,,,)", (,,,) 1 2 3 4)
  print ("(,,,,)", (,,,,) 1 2 3 4 5)
  print ("(,,,,,)", (,,,,,) 1 2 3 4 5 6)
  print ("(,,,,,,)", (,,,,,,) 1 2 3 4 5 6 7)
  print ("7 equal", (,,,,,,) 1 2 3 4 5 6 7 == (1, 2, 3, 4, 5, 6, 7))
  print ("fst/snd", fst (1, 'a'), snd (1, 'a'))
  print ("partial", map ((,) 0) "ab")
  print ("zipWith", zipWith (,) [1, 2] "xy")
  print ("zipWith3", zipWith3 (,,) [1, 2] "xy" [True, False])
  print ("nested", ((1, 2), (3, (4, 5))))
  print ("nested select", fst (snd ((1, 2), (3, (4, 5)))), snd (snd (snd ((1, 2), (3, (4, 5))))))
  print ("curry", curry fst 1 'x', uncurry (+) (3, 4))
  print ("swap", swapPair (1, "one"), third (1, 2, "three"))
  print ("eq", (1, 'a') == (1, 'a'), (1, 'a') == (1, 'b'), (1, 'a') /= (2, 'a'))
  print ("ord", (1, 'b') < (2, 'a'), (1, 'b') < (1, 'c'), compare (2, 'a') (1, 'z'))
  print ("maximum", maximum [(1, 'c'), (3, 'a'), (2, 'b')])
  print ("triple ord", compare (1, 2, 3) (1, 2, 4), (1, 2, 3) < (1, 3, 0))
  print ("7 ord", (1, 2, 3, 4, 5, 6, 7) < (1, 2, 3, 4, 5, 6, 8), max (1, 2, 3, 4, 5, 6, 7) (1, 2, 3, 4, 5, 7, 0))
  print ("pattern", let (a, b, c) = (1, 'x', True) in (c, b, a))
  print ("lazy components", fst (1, undefined), case (undefined, 2) of { (_, y) -> y })
  print ("bounds", (minBound, maxBound) :: (Bool, Bool))
  print ("tuple of functions", fst (succ, pred) 5, snd (succ, pred) 5)
  print ("mixed types", (1, 'c', "s", True, 2.5, [1], Just ()))
  print ("spaces", ( , ) 1 2, ( , , ) 1 2 3)
