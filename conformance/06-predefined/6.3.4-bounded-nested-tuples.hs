-- Haskell 2010 Report 6.3.4, 6.1.4: Bounded for tuples is componentwise, however they are nested
module Main (main) where

main :: IO ()
main = do
  print ("min nested", minBound :: (Bool, (Char, Int), Ordering))
  print ("max nested", maxBound :: (Bool, (Char, Int), Ordering))
  print ("unit inside", (minBound, maxBound) :: (((), Bool), (Bool, ())))
  print ("triple of pairs", minBound :: ((Bool, Bool), (Ordering, Ordering), (Int, Char)))
  print ("deep", maxBound :: (Bool, (Bool, (Bool, (Bool, Ordering)))))
  print ("component", fst (maxBound :: (Ordering, Int)), snd (minBound :: (Int, Ordering)))
  print ("equal to components", (maxBound :: (Bool, Char)) == (maxBound, maxBound))
