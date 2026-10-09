-- Haskell 2010 Report 6.1.5: the unit datatype () and its Eq, Ord, Bounded, Enum, Read, Show instances
isUnit :: () -> Bool
isUnit () = True

ignore :: a -> ()
ignore _ = ()

main :: IO ()
main = do
  print ()
  print ("show", show (), showsPrec 11 () "", show [(), ()], show (Just ()))
  print ("eq", () == (), () /= ())
  print ("ord", compare () (), () < (), () <= (), max () (), min () ())
  print ("bounded", minBound :: (), maxBound :: ())
  print ("enum", fromEnum (), toEnum 0 :: (), [() ..], [() .. ()])
  print ("enumFromThenTo", take 3 [(), () ..], length (take 5 (enumFromThen () ())))
  print ("read", read "()" :: (), read " () " :: (), read "(())" :: ())
  print ("reads", reads "() rest" :: [((), String)])
  print ("pattern", isUnit (), isUnit (ignore 'x'))
  print ("ignore does not force", ignore (undefined :: Int))
  print ("case", case () of { () -> "matched" })
  u <- return ()
  print ("from IO", u)
  r <- putStr ""
  print ("putStr result", r)
  print ("in structures", ((), [()], ((), ())))
