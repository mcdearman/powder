-- Haskell 2010 Report 7.2 / 3.14: do notation in IO is sugar for (>>=) and (>>); each form is run next to its translation
step :: String -> Int -> IO Int
step label n = do
  putStrLn ("step " ++ label)
  return n

sugared :: IO Int
sugared = do
  a <- step "a" 1
  step "unused" 100
  let b = a + 10
      c = b * 2
  d <- step "d" c
  return (a + b + c + d)

desugared :: IO Int
desugared =
  step "a" 1 >>= \a ->
    step "unused" 100 >>
      let b = a + 10
          c = b * 2
       in step "d" c >>= \d ->
            return (a + b + c + d)

main :: IO ()
main = do
  x <- sugared
  y <- desugared
  print ("same result", x, y, x == y)
  -- do {e} = e
  do putStrLn "single expression do"
  -- explicit braces and semicolons
  do { putStrLn "braces"; putStrLn "and semicolons" }
  -- nested do blocks and shadowing
  v <- do
    v <- step "inner" 5
    v <- return (v + 1)
    return (v * 2)
  print ("nested with shadowing", v)
  -- pattern bindings
  (p, q) <- return ('p', "q")
  [r, s] <- return [1, 2 :: Int]
  print ("patterns", p, q, r, s)
  -- let with a function and a pattern
  let twice f = f . f
      (lo, hi) = (twice (subtract 1) r, twice (+ 1) s)
  print ("let", lo, hi)
  -- the value of a do block is the value of its last expression
  z <- do
    _ <- step "discarded" 0
    step "kept" 7
  print ("last expression", z)
  -- if and case inside do
  if z > 5 then putStrLn "if: big" else putStrLn "if: small"
  case v of
    12 -> putStrLn "case: twelve"
    _ -> putStrLn "case: other"
  -- a statement that is a non-unit action: its result is dropped
  step "result dropped" 1
  return ()
  putStrLn "return () in the middle does not end the block"
