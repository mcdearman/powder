-- Haskell 2010 Report 3.12 / 3.3 / 4.5.4: let-bound variables are generalised, lambda-bound variables are monomorphic
module Main (main) where

main :: IO ()
main = do
  let ident x = x
  print ("let function at three types", ident 1, ident 'c', ident "s")
  let pair :: a -> (a, a)
      pair x = (x, x)
  print ("let with signature", pair True, pair 'q')
  print ("let expression", let k x _ = x in (k 1 'a', k 'b' 2))
  print ("lambda value generalised", let f = \x -> [x] in (f 1, f 'a'))
  print ("empty list generalised", let e = [] in (1 : e, 'a' : e))
  print ("overloaded function binding", let double x = x + x in (double 2, double 1.5))
  print ("higher order", let twice f = f . f in (twice (+ 1) 0, twice ('x' :) ""))
  print ("compose at two types", let app f x = f x in (app not True, app succ 'a'))
  -- a lambda-bound variable may be used repeatedly, but only at one type
  print ("lambda monomorphic", (\f -> (f 1, f 2)) (+ 1))
  print ("lambda takes instance", (\f -> f 'x') ident, (\f -> f True) ident)
  -- a let inside a lambda generalises over what the lambda does not fix
  print ("let under lambda", (\x -> let g y = (x, y) in (g 'a', g True)) 1)
  print ("where", viaWhere)
  print ("polymorphic pair of functions", let (f, g) = (fst, snd) in (f (1, 'a'), g (1, 'a'), f ("s", ()), g ("s", ())))
  print ("const used twice", let c = const in (c 1 'x', c 'y' 2))
  where
    viaWhere = (single 1, single "one")
    single x = [x]
