-- Haskell 2010 Report 3.12: pattern bindings in let are matched lazily (implicit ~)
module Main (main) where

main :: IO ()
main = do
  print ("tuple", let (q, r) = 17 `divMod` 5 in (q, r))
  print ("undefined tuple unused", let (a, b) = undefined :: (Int, Int) in "unused")
  print ("failing cons unused", let (x : _) = [] :: [Int] in "unused")
  print ("failing Just unused", let Just v = Nothing :: Maybe Int in "unused")
  print ("failing literal unused", let 1 = 2 :: Int in "unused")
  print ("failing list unused", let [a, b] = [1, 2, 3 :: Int] in "unused")
  print ("list", let [a, b, c] = "xyz" in [c, b, a])
  print ("partial bottom", let [a, b] = [1, undefined :: Int] in a)
  print ("nested", let ((a, b), [c, d]) = ((1, 2), [3, 4]) in a + b + c + d)
  print ("as", let whole@(h : t) = "abc" in (whole, h, t))
  print ("constructor", let Just (n, s) = Just (1, "one") in (s, n))
  print ("explicit lazy", let ~(a, b) = (1, 2) in a + b)
  print ("wildcard binding", let _ = undefined :: Int in "wild")
  print ("bottom inside matched tuple", let (a, _) = (5, undefined :: Int) in a)
  print ("several", let { (a, b) = (1, 2); (c, d) = (b, a) } in (a, b, c, d))
  print ("infinite", let (x : y : _) = [1 ..] in x + y)
  -- let p = e1 in e0  =  case e1 of ~p -> e0
  print ("as case with ~", (let (a, b) = (3, 4) in a * b) == (case (3, 4) of { ~(a, b) -> a * b }))
  print ("case with ~ on bottom", case (undefined :: (Int, Int)) of { ~(_, _) -> "lazy" })
  print ("where is lazy too", lazyWhere)
  where
    lazyWhere = "where"
      where
        (_p, _q) = undefined :: (Int, Int)
