-- Haskell 2010 Report 3.13: each case alternative may have a where clause scoping over all its guards and bodies
module Main (main) where

f :: Int -> String
f n = case n of
  0 -> zero
    where zero = "zero"
  k | even k -> evenS
    | k > 100 -> big
    | otherwise -> other
    where
      evenS = "even " ++ show half
      half = k `div` 2
      big = "big"
      other = "odd " ++ show k

area :: (Int, Int) -> Int
area dims = case dims of
  (w, h) -> scaled
    where
      scaled = base * factor
      base = w * h
      factor = 2

sameName :: Either Int Int -> Int
sameName e = case e of
  Left x -> v where v = x + 1
  Right x -> v where v = x * 10

guardsSeeWhere :: Int -> String
guardsSeeWhere n = case n of
  m | sq > 50 -> "big square"
    | sq > 10 -> "medium square"
    where sq = m * m
  _ -> "small square"

main :: IO ()
main = do
  print ("f", map f [0, 4, 7, 101, 102])
  print ("area", area (3, 4))
  print ("same name in two alternatives", sameName (Left 1), sameName (Right 1))
  print ("guards see where, then fall through", map guardsSeeWhere [8, 4, 2])
  print ("inline", case 5 of { x -> y where { y = x + 1 } })
  print ("where shadows outer", let y = 1 in case 5 of { x -> y where { y = x * 100 } })
  print ("where function", case "abc" of { s -> twice s where { twice t = t ++ t } })
