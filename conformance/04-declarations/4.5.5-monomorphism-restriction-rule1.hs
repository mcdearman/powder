-- Haskell 2010 Report 4.5.5 Rule 1: a simple pattern binding without a signature is not generalised over constrained type variables
module Main (main) where

-- restricted: monomorphic, and used at exactly one type (Int) in this module
plus = (+)

-- function syntax: generalised, usable at several types
plusFn x y = x + y

-- explicit signature: generalised
plusSig :: Num a => a -> a -> a
plusSig = (+)

-- no constrained type variables: generalised even though it is a simple pattern binding
ident = id

-- restricted, used only at Bool
showIt = show

-- a non-simple pattern binding is always restricted: lo is used only at Int, hi only at Char
(lo, hi) = (minBound, maxBound)

main :: IO ()
main = do
  print ("plus", plus (1 :: Int) 2)
  print ("plus again at the same type", plus 3 (4 :: Int))
  print ("plusFn", plusFn (1 :: Int) 2, plusFn (1.5 :: Double) 2)
  print ("plusSig", plusSig (1 :: Int) 2, plusSig (0.5 :: Double) 2)
  print ("ident", ident 'c', ident True)
  print ("showIt", showIt True, showIt False)
  print ("pattern binding", lo :: Int, hi :: Char)
  print ("local restricted", let add = (+) in add (1 :: Int) 2)
  print ("local function syntax", let sq x = x * x in (sq (3 :: Int), sq (1.5 :: Double)))
  print ("local restricted value", let n = 5 in (n + (1 :: Int), n * 2))
  print ("local unconstrained", let e = [] in (length ('a' : e), length (True : e)))
  print ("local with signature", let n :: Num a => a; n = 5 in (n + (1 :: Int), n + (0.5 :: Double)))
