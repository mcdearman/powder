-- Reading values from text, a default declaration, and files.
module Main (main) where

default (Int, Double)

data Colour = Red | Green | Blue deriving (Show, Eq, Enum, Bounded)

total :: String -> Int
total = sum . map read . words

main :: IO ()
main = do
  print (read "42" :: Int, read " -7 " :: Integer, read "3.25" :: Double, read "1e3" :: Double)
  print (read "[1,2,3]" :: [Int], read "(1,\"two\")" :: (Int, String), read "Just (-3)" :: Maybe Int)
  print (read "'x'" :: Char, read "[True, False]" :: [Bool], (reads :: ReadS Int) "12 rest", (reads :: ReadS Int) "none")
  print (total "1 2 3 4", lex "  hello world", lex "<= 3")
  -- With the default declaration, a number of no particular type is an Int.
  print (maxBound - 2 ^ 62 + (2 ^ 62 - 1) == (maxBound :: Int) - 1, 7 / 2)
  writeFile "/tmp/powder-reading.txt" "first\n"
  appendFile "/tmp/powder-reading.txt" "second\n"
  text <- readFile "/tmp/powder-reading.txt"
  print (lines text, [minBound .. maxBound :: Colour])
