-- Haskell 2010 Report 9: class Monad ((>>=) (>>) return fail) at IO; fail in IO raises an IOError
module Main (main) where

import System.IO.Error (catchIOError)

twice :: Int -> IO Int
twice n = return (n * 2)

announce :: String -> IO Int
announce s = putStrLn ("running " ++ s) >> return (length s)

main :: IO ()
main = do
  a <- return 5
  print ("return", a :: Int)
  b <- return 5 >>= twice
  print (">>=", b)
  c <- return 1 >>= twice >>= twice >>= twice
  print (">>= chain", c)
  d <- announce "first" >> announce "second!"
  print (">> keeps second", d)
  return 7 >>= print
  putStr "x" >> putStr "y" >> putStrLn "z"
  e <- (announce "left" >>= \n -> announce "middle" >>= \m -> return (n + m))
  print ("nested binds", e)
  let unused = putStrLn "never printed"
  f <- return (length [unused, unused])
  print ("actions are values", f)
  g <- catchIOError (fail "boom" >> return "not caught") (\_ -> return "caught")
  print ("fail in IO", g)
  h <- catchIOError (putStrLn "before fail" >> fail "boom" >> putStrLn "after fail" >> return 0) (\_ -> return (1 :: Int))
  print ("fail skips the rest", h)
  i <- catchIOError (do { Just x <- return (Nothing :: Maybe Int); return x }) (\_ -> return (-1))
  print ("pattern failure in IO do", i)
  j <- do { (x, y) <- return (3 :: Int, 4 :: Int); return (x + y) }
  print ("pattern success", j)
  print ("done", ())
