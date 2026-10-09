-- Haskell 2010 Report 3.6: if exp [;] then exp [;] else exp (optional semicolons, so then/else may align with if in a do block)
module Main (main) where

pick :: Int -> IO ()
pick n = do
  if even n
  then putStrLn ("even " ++ show n)
  else putStrLn ("odd " ++ show n)
  putStrLn "after if"

main :: IO ()
main = do
  pick 1
  pick 2
  do { if True ; then putStrLn "explicit then" ; else putStrLn "explicit else" ; putStrLn "explicit after" }
  x <- do
    if False
    then return 'a'
    else return 'b'
  print ("result", x)
  print ("list monad", do { n <- [1, 2, 3] ; if odd n ; then [n] ; else [] })
