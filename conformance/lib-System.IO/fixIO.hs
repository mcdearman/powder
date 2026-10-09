-- Haskell 2010 Report 41.1 (System.IO): IO and fixIO (the result of the action is fed back lazily as its
-- own argument; the action's effects happen once)
module Main (main) where

import System.IO

data Node = Node Int Node

label :: Node -> Int
label (Node n _) = n

next :: Node -> Node
next (Node _ t) = t

main :: IO ()
main = do
  xs <- fixIO (\ys -> return (1 : take 3 ys))
  print ("knot through take", xs :: [Int])
  ones <- fixIO (\ys -> return (1 : ys))
  print ("infinite list of ones", take 5 (ones :: [Int]))
  nats <- fixIO (\ys -> return (0 : map (+ 1) ys))
  print ("naturals", take 6 (nats :: [Int]))
  fibs <- fixIO (\fs -> return (0 : 1 : zipWith (+) fs (tail fs)))
  print ("fibonacci", take 12 (fibs :: [Integer]))
  five <- fixIO (\_ -> return (5 :: Int))
  print ("argument unused", five)
  once <- fixIO (\ys -> putStrLn "effect happens once" >> return ('a' : take 2 ys))
  print ("with an effect", once)
  (n, s) <- fixIO (\ ~(_, str) -> return (length str, "abc"))
  print ("lazy tuple knot", n, s)
  ring <- fixIO (\self -> return (Node 1 (Node 2 (Node 3 self))))
  print ("cyclic structure", map label (take 7 (iterate next ring)))
  table <- fixIO (\t -> return [if i < 2 then toInteger i else t !! (i - 1) + t !! (i - 2) | i <- [0 .. 30 :: Int]])
  print ("self-referential table", table !! 30)
  line <- fixIO (\l -> do { putStr "inside the action: "; putStrLn "the argument is not forced"; return ("x" ++ take 0 l) })
  print ("argument never forced", line)
