-- Haskell 2010 Report 3.15 / 4.2.1: records with strict fields; construction, selection, update and omitted lazy fields
module Main (main) where

data S = S { sx :: !Int, sy :: Int, sz :: !String }
  deriving (Eq, Show)

data Mixed = Mixed !Int Int
  deriving (Eq, Show)

data Q a = Q { strictPart :: !a, lazyPart :: a }
  deriving (Eq, Show)

lazySecond :: Mixed -> Int
lazySecond (Mixed a _) = a

main :: IO ()
main = do
  let s = S { sx = 1, sy = 2, sz = "s" }
  print ("construct", s)
  print ("any order", S { sz = "s", sy = 2, sx = 1 } == s)
  print ("positional", S 1 2 "s" == s)
  print ("select", sx s, sy s, sz s)
  print ("update strict", s { sx = 10 })
  print ("update lazy", s { sy = 20 })
  print ("update both", s { sx = 3, sz = "t" })
  -- the lazy field may be omitted or bottom; the strict ones are given
  print ("lazy field omitted", sx (S { sx = 5, sz = "z" }), sz (S { sz = "z", sx = 5 }))
  print ("lazy field bottom", sx (S 5 undefined "z"))
  print ("lazy positional bottom", lazySecond (Mixed 7 undefined))
  print ("strict field evaluated value", sx (S (2 + 3) 0 ("a" ++ "b")), sz (S (2 + 3) 0 ("a" ++ "b")))
  -- a strict field is forced only to weak head normal form
  print ("strict field whnf only", length (sz (S 0 0 [undefined, undefined])))
  print ("polymorphic strict", strictPart (Q 'a' undefined), strictPart (Q (Just undefined :: Maybe Int) Nothing) == Nothing)
  print ("pattern", case s of { S { sx = a, sz = c } -> (a, c) })
  print ("constructor as function", zipWith3 S [1, 2] [3, 4] ["a", "b"])
