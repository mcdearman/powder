-- Haskell 2010 Report 3.14: a failed pattern match in a do binding calls fail (Nothing, [], or an IOError in IO)
module Main (main) where

import System.IO.Error (catchIOError)

firsts :: [[Int]] -> [Int]
firsts xss = do
  (x : _) <- xss
  return x

justHead :: Maybe [Int] -> Maybe Int
justHead m = do
  (x : _) <- m
  return x

keyed :: [(Int, Char)] -> String
keyed ps = do
  (1, c) <- ps
  return c

lazyHead :: Maybe [Int] -> Maybe Int
lazyHead m = do
  ~(_ : _) <- m
  return 0

both :: Maybe (Maybe Int, Maybe Int) -> Maybe Int
both m = do
  (Just a, Just b) <- m
  return (a + b)

main :: IO ()
main = do
  print ("list skips failures", firsts [[1], [], [2, 3], []])
  print ("maybe ok", justHead (Just [7, 8]))
  print ("maybe fails", justHead (Just []))
  print ("maybe nothing", justHead Nothing)
  print ("literal pattern", keyed [(1, 'a'), (2, 'b'), (1, 'c')])
  print ("irrefutable never fails", lazyHead (Just []))
  print ("nested ok", both (Just (Just 1, Just 2)))
  print ("nested fails", both (Just (Just 1, Nothing)), both (Just (Nothing, Just 2)))
  print ("Just in list", do { Just x <- [Just 1, Nothing, Just 3]; return (x * 2) })
  print ("Left in list", do { Left x <- [Left 'a', Right 1, Left 'b']; [x, x] })
  print ("failure then more", do { Just x <- [Nothing, Just 1]; y <- "ab"; return (x, y) })
  print ("variable never fails", do { x <- [Nothing, Just 1]; return (x == Nothing) })
  print ("explicit fail", fail "ignored" :: Maybe Int, fail "ignored" :: [Int])
  catchIOError
    (do { (x : _) <- return ([] :: [Int]); print ("not reached", x) })
    (\_ -> putStrLn "IO: pattern failure raised an IOError")
  r <- catchIOError (do { Just v <- return (Just 3); return (v :: Int) }) (\_ -> return 0)
  print ("IO: pattern success", r)
  s <- catchIOError (do { [a, b] <- return "xyz"; return [b, a] }) (\_ -> return "caught")
  print ("IO: pattern failure handled", s)
