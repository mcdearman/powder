-- Numbers of a fixed size, deriving Read and Ix, the end of the input,
-- and the program's arguments.
import Data.Array
import Data.Ix
import System.Environment
import System.IO

data Colour = Red | Green | Blue deriving (Eq, Ord, Show, Read, Enum, Bounded, Ix)

data Cell = Cell Colour Int deriving (Eq, Ord, Show, Read, Ix)

data Shape = Circle {radius :: Int} | Pair Int Int | Shape :+: Shape deriving (Eq, Show, Read)

infixl 6 :+:

third :: Float
third = 1 / 3

countLines :: Int -> IO Int
countLines n = do
  done <- isEOF
  if done then return n else getLine >> countLines (n + 1)

main :: IO ()
main = do
  print (maxBound + 1 == (minBound :: Int), 2 ^ 63 :: Int, 2 ^ 64 :: Integer, (maxBound :: Int) * 2)
  print (third, realToFrac third :: Double, 16777216 + 1 :: Float, sqrt 2 :: Float, floatDigits third)
  print (read "Blue" :: Colour, read "[Red,Green]" :: [Colour], read " Cell Green (-3)" :: Cell)
  print (read "Circle {radius = 2} :+: Pair 1 2" :: Shape, (reads :: ReadS Shape) "Pair 1")
  print (range (Red, Blue), index (Green, Blue) Blue, inRange (Red, Green) Blue)
  print (range (Cell Red 1, Cell Green 2), index (Cell Red 0, Cell Blue 9) (Cell Green 3))
  let counts = accumArray (+) 0 (Red, Blue) [(Blue, 2), (Red, 1), (Blue, 5)] :: Array Colour Int
  print (counts ! Blue, assocs counts)
  name <- getProgName
  args <- getArgs
  print (not (null name), args)
  n <- countLines 0
  print n
