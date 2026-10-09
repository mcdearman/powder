-- Haskell 2010 Report 3.14: do expressions in the Maybe monad
module Main (main) where

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv a b = Just (a `div` b)

calc :: Int -> Int -> Int -> Maybe Int
calc a b c = do
  x <- safeDiv a b
  y <- safeDiv x c
  return (x + y)

lookupBoth :: String -> String -> [(String, Int)] -> Maybe (Int, Int)
lookupBoth k1 k2 env = do
  v1 <- lookup k1 env
  v2 <- lookup k2 env
  let total = v1 + v2
  Just (total, v1 * v2)

env0 :: [(String, Int)]
env0 = [("a", 2), ("b", 5)]

main :: IO ()
main = do
  print ("ok", calc 100 5 2)
  print ("first fails", calc 100 0 2)
  print ("second fails", calc 100 5 0)
  print ("lookup both", lookupBoth "a" "b" env0)
  print ("lookup missing", lookupBoth "a" "z" env0, lookupBoth "z" "a" env0)
  print ("then", do { Just (); Just 'x' }, do { Nothing :: Maybe (); Just 'x' })
  print ("ends in Nothing", do { _ <- Just 1; Nothing } :: Maybe Char)
  -- Nothing short-circuits: the rest is never evaluated
  print ("short circuit", do { _ <- Nothing :: Maybe Int; undefined } :: Maybe Int)
  print ("tuple pattern", do { (a, b) <- Just (1, 'x'); return (b, a) })
  print ("nested maybe", do { m <- Just (Just 3); n <- m; return (n + 1) })
  print ("nested nothing", do { m <- Just (Nothing :: Maybe Int); n <- m; return (n + 1) })
  print ("let", do { x <- Just 3; let { y = x * x }; return (x + y) })
  print ("if", do { x <- Just 3; if x > 2 then Just "big" else Nothing })
  print ("case", do { x <- Just 3; case x of { 3 -> Just "three"; _ -> Nothing } })
  print ("mapM", mapM (safeDiv 100) [1, 2, 5], mapM (safeDiv 100) [1, 0, 5])
  print ("sequence", sequence [Just 1, Just 2], sequence [Just 1, Nothing])
