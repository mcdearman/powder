-- Haskell 2010 Report 3.9: the unit expression () and parenthesized expressions (e) = e
module Main (main) where

unitFn :: () -> Int
unitFn () = 1

main :: IO ()
main = do
  print ("unit", ())
  print ("eq", () == (), () /= ())
  print ("ord", compare () (), () < (), () <= ())
  print ("bounds", minBound :: (), maxBound :: ())
  print ("enum", fromEnum (), [() ..], toEnum 0 :: ())
  print ("show", show ())
  print ("pattern", unitFn (), case () of { () -> 'u' })
  print ("lazy unit argument", const 2 (undefined :: ()))
  print ("paren", (5), ((5)), (((5))))
  print ("paren expr", (1 + 2) * 3, 1 + (2 * 3), ((1 + 2)) * ((3)))
  print ("paren function", (negate) 5, (succ) ((4)), ((+)) 1 2)
  print ("paren section", ((+ 1)) 2)
  print ("paren constructor", (Just) 1, ((:)) 1 [])
  print ("paren lambda", ((\x -> x)) 'p')
  print ("unit list", [(), ()], length [(), (), ()])
  print ("nested unit", ((), ((), ())))
  print ("paren unit", (()), (()) == ())
  print ("paren signature", (3 :: Int), ((3) :: Int))
  print ("unit in tuple", fst ((), 1), snd (1, ()))
  r <- return ()
  print ("return unit", r)
  print ("paren string", ("abc"), (('x')))
