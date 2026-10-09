-- Haskell 2010 Report 9 (PreludeIO): readIO and readLn; a failed parse raises an IOError instead of calling error
-- (see the sibling .in file)
module Main (main) where

import System.IO.Error (catchIOError)

tryReadInt :: String -> IO (Maybe Int)
tryReadInt s = catchIOError (fmap Just (readIO s)) (\_ -> return Nothing)

main :: IO ()
main = do
  a <- readIO "42"
  print ("readIO Int", a + (1 :: Int))
  b <- readIO "  [1,2,3]  "
  print ("readIO list with spaces", b :: [Int])
  c <- readIO "(True,'x',\"s\")"
  print ("readIO tuple", c :: (Bool, Char, String))
  d <- readIO "Just (-2.5)"
  print ("readIO Maybe Double", d :: Maybe Double)
  r1 <- tryReadInt "12"
  r2 <- tryReadInt "twelve"
  r3 <- tryReadInt ""
  r4 <- tryReadInt "12 13"
  r5 <- tryReadInt "(12"
  print ("readIO failures are IOErrors", [r1, r2, r3, r4, r5])
  n <- readLn
  print ("readLn Int", n * (2 :: Int))
  x <- readLn
  print ("readLn Double", x / (2 :: Double))
  p <- readLn
  print ("readLn pair", p :: (Int, String))
  xs <- readLn
  print ("readLn list", sum (xs :: [Integer]))
  m <- readLn
  print ("readLn Maybe with spaces", m :: Maybe Bool)
  bad <- catchIOError (fmap Just (readLn :: IO Int)) (\_ -> return Nothing)
  print ("readLn failure is an IOError", bad)
  after <- readLn
  print ("readLn after failure reads the next line", after :: Char)
