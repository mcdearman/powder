-- Haskell 2010 Report 3.14: the translation of do expressions into >>=, >> and let
module Main (main) where

half :: Int -> Maybe Int
half n = if even n then Just (n `div` 2) else Nothing

main :: IO ()
main = do
  -- do {e} = e
  print ("single", do { Just 1 } == Just 1, do { [1, 2] } == [1, 2])
  -- do {e; stmts} = e >> do {stmts}
  print ("then", do { Just 'a'; Just 'b' } == (Just 'a' >> Just 'b'))
  print ("then list", do { "ab"; [1, 2] } == ("ab" >> [1, 2]), do { "ab"; [1, 2] })
  print ("then nothing", do { Nothing; Just 'b' } == (Nothing >> Just 'b'), do { half 3; Just 'b' })
  -- do {p <- e; stmts} = let { ok p = do {stmts}; ok _ = fail "..." } in e >>= ok
  print ("bind", do { x <- half 8; half x } == (half 8 >>= \x -> half x), do { x <- half 8; half x })
  print ("bind two", do { x <- [1, 2]; y <- "ab"; return (x, y) }
                     == ([1, 2] >>= \x -> "ab" >>= \y -> return (x, y)))
  print ("bind pattern", do { (x : _) <- Just [1, 2]; return x }
                         == (let { ok (x : _) = return x; ok _ = fail "no match" } in Just [1, 2] >>= ok))
  print ("bind pattern fails", do { (x : _) <- Just []; return (x :: Int) }
                               == (let { ok (x : _) = return x; ok _ = fail "no match" } in Just [] >>= ok))
  print ("bind pattern list", do { (1, c) <- [(1, 'a'), (2, 'b'), (1, 'c')]; return c }
                              == (let { ok (1, c) = return c; ok _ = fail "no match" } in [(1, 'a'), (2, 'b'), (1, 'c')] >>= ok))
  -- do {let decls; stmts} = let decls in do {stmts}
  print ("let", do { let { k = 10 }; x <- [1, 2]; return (x + k) }
                == (let { k = 10 } in do { x <- [1, 2]; return (x + k) }))
  print ("let value", do { let { k = 10 }; x <- [1, 2]; return (x + k) })
  -- the monad laws seen through do
  print ("left identity", do { x <- return 4; half x } == half 4)
  print ("right identity", do { x <- half 4; return x } == half 4)
  print ("associativity", do { y <- do { x <- half 8; half x }; half y } == do { x <- half 8; do { y <- half x; half y } })
  -- a comprehension is the corresponding do in the list monad
  print ("comprehension", [(x, y) | x <- [1, 2], y <- "ab"] == do { x <- [1, 2]; y <- "ab"; return (x, y) })
