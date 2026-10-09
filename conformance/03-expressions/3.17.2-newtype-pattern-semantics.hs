-- Haskell 2010 Report 3.17.2 / 4.2.3: matching a newtype constructor pattern does not evaluate the value; a data constructor pattern does
module Main (main) where

newtype N = N Int

data D1 = D1 Int

newtype P = P (Int, Int)

newtype B = B Bool

newtype NN = NN N

n :: N -> Int
n (N _) = 42

d1 :: D1 -> Int
d1 (D1 _) = 42

pairN :: P -> Int
pairN (P ~(a, b)) = 7

unN :: N -> Int
unN (N i) = i

main :: IO ()
main = do
  -- n _|_ = 42 for a newtype
  print ("newtype on bottom", n undefined)
  -- d1 (D1 _|_) = 42 for a datatype with a lazy field
  print ("data with bottom field", d1 (D1 undefined))
  print ("newtype of bottom", n (N undefined))
  print ("case newtype on bottom", case (undefined :: N) of { N _ -> "matched" })
  print ("lambda newtype on bottom", (\(N _) -> 5) undefined)
  print ("newtype variable bound lazily", (\(N i) -> 6) undefined)
  print ("nested newtypes on bottom", case (undefined :: NN) of { NN (N _) -> "matched" })
  print ("newtype then lazy pattern", pairN undefined)
  print ("lazy newtype literal", (\ ~(B True) -> "lazy") undefined)
  print ("newtype inside data forces the data only", case Just (undefined :: N) of { Just (N _) -> "matched"; Nothing -> "nothing" })
  -- ordinary use
  print ("unwrap", unN (N 3), map unN [N 1, N 2])
  print ("literal under newtype", [case b of { B True -> 't'; B False -> 'f' } | b <- [B True, B False]])
  print ("newtype of pair", case P (1, 2) of { P (a, b) -> a + b })
  print ("constructor as function", map (unN . N) [4, 5])
  print ("let binding", let N k = N 9 in k)
  print ("as pattern", case N 3 of { whole@(N k) -> k + unN whole })
