-- Haskell 2010 Report 6.1.4: Eq, Ord, Bounded, Read and Show instances for tuples of sizes 2 to 7
type T2 = (Int, Char)
type T3 = (Int, Char, Bool)
type T4 = (Int, Char, Bool, ())
type T5 = (Int, Char, Bool, (), Ordering)
type T6 = (Int, Char, Bool, (), Ordering, String)
type T7 = (Int, Char, Bool, (), Ordering, String, Integer)

t2 :: T2
t2 = (1, 'a')

t3 :: T3
t3 = (1, 'a', True)

t4 :: T4
t4 = (1, 'a', True, ())

t5 :: T5
t5 = (1, 'a', True, (), EQ)

t6 :: T6
t6 = (1, 'a', True, (), EQ, "s")

t7 :: T7
t7 = (1, 'a', True, (), EQ, "s", 99)

main :: IO ()
main = do
  print t2
  print t3
  print t4
  print t5
  print t6
  print t7
  putStrLn (showsPrec 11 t2 "")
  putStrLn (show ((-1, -2) :: (Int, Int)))
  putStrLn (show ((1, (2, 3)), [(4, 5)]) :: String)
  print ("eq 2-4", t2 == t2, t2 /= (1, 'b'), t3 == (1, 'a', False), t4 == (1, 'a', True, ()))
  print ("eq 5-7", t5 == t5, t6 /= (1, 'a', True, (), EQ, "t"), t7 == (1, 'a', True, (), EQ, "s", 99))
  print ("ord lexicographic 2", (1, 'b') < (2 :: Int, 'a'), (2, 'a') < (2 :: Int, 'b'), compare t2 (0, 'z'))
  print ("ord 3-4", t3 > (1, 'a', False), compare t3 t3, t4 <= t4, compare t4 (1, 'b', False, ()))
  print ("ord 5-6", compare t5 (1, 'a', True, (), GT), t6 < (1, 'a', True, (), EQ, "sa"))
  print ("ord 7", compare t7 (1, 'a', True, (), EQ, "s", 100), t7 >= (1, 'a', True, (), EQ, "r", 1000))
  print ("max/min", max t2 (1, 'b'), min t3 (0, 'z', True))
  print ("read 2-3", read "(1,'a')" :: T2, read " ( 1 , 'a' , True ) " :: T3)
  print ("read 4-5", read "(1,'a',True,())" :: T4, read "(1,'a',True,(),EQ)" :: T5)
  print ("read 6", read "(1,'a',True,(),EQ,\"s\")" :: T6)
  print ("read 7", read "(1,'a',True,(),EQ,\"s\",99)" :: T7)
  print ("read . show", read (show t7) == t7, read (show t5) == t5)
  print ("read nested", read "((1,'x'),[(2,'y')])" :: (T2, [T2]))
  print ("bounded 2", minBound :: (Bool, Ordering), maxBound :: (Bool, Ordering))
  print ("bounded 3", minBound :: (Bool, Ordering, ()), maxBound :: (Bool, Ordering, ()))
  print ("bounded 4", minBound :: (Bool, Ordering, (), Char), maxBound :: (Bool, Bool, Bool, Bool))
  print ("bounded 5", maxBound :: (Bool, Ordering, (), Bool, Ordering))
  print ("bounded 6", minBound :: (Bool, Ordering, (), Bool, Ordering, ()))
  print ("bounded 7", maxBound :: (Bool, Ordering, (), Bool, Ordering, (), Bool))
  print ("bounded with Int", fst (minBound :: (Int, Bool)) == minBound, snd (maxBound :: (Int, Char)) == maxBound)
