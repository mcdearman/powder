-- Haskell 2010 Report 3.14: nested do blocks, let statements, and shadowing of earlier bindings
module Main (main) where

nested :: [(Int, Char)]
nested = do
  x <- [1, 2]
  ys <- return (do
    c <- "ab"
    return (x, c))
  ys

shadow :: Maybe Int
shadow = do
  x <- Just 1
  x <- Just (x + 10)
  let x' = x * 2
  x <- Just (x' + 1)
  return x

letStmts :: [Int]
letStmts = do
  let a = 1
      b = a + 1
  c <- [a, b]
  let double n = n * 2
      d = double c
  let { e = d + 1; f = e + 1 }
  [d, e, f]

polyLet :: Maybe (String, [Int])
polyLet = do
  let dup x = x ++ x
  return (dup "ab", dup [1])

main :: IO ()
main = do
  print ("nested", nested)
  print ("shadow", shadow)
  print ("let statements", letStmts)
  print ("poly let", polyLet)
  let v = 1 :: Int
  v <- return (v + 1)
  print ("io shadow", v)
  w <- do
    u <- return 10
    t <- do
      return (u + v)
    return (t * 2)
  print ("io nested", w)
  let pair = do { a <- Just 1; b <- do { c <- Just 2; return (c * 10) }; return (a, b) }
  print ("inline nested", pair)
  let recursive = 1 : map (* 2) recursive
  print ("let in do is recursive", take 5 recursive)
  let (q, r) = 17 `divMod` 5
      [h, _] = "hi"
  print ("pattern let", q, r, h)
  do
    let inner = "inner block"
    putStrLn inner
  print ("do as argument", maybe 0 (+ 1) (do { n <- Just 5; return (n * 2) }))
