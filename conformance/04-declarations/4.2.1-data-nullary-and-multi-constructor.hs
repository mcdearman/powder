-- Haskell 2010 Report 4.2.1: algebraic datatypes with nullary and multi-argument constructors
module Main (main) where

data Color = Red | Green | Blue

data Shape = Circle Int | Rect Int Int | Dot

data Single = Single

data Wrapper = Wrapper Color Shape

-- a type and a constructor may share a name; so may constructors and type names of other types
data Tag = Tag Int | Color

colorName :: Color -> String
colorName Red = "red"
colorName Green = "green"
colorName Blue = "blue"

nextColor :: Color -> Color
nextColor Red = Green
nextColor Green = Blue
nextColor Blue = Red

area :: Shape -> Int
area (Circle r) = 3 * r * r
area (Rect w h) = w * h
area Dot = 0

corners :: Shape -> Int
corners (Rect _ _) = 4
corners _ = 0

singleName :: Single -> String
singleName Single = "single"

wrapperInfo :: Wrapper -> (String, Int)
wrapperInfo (Wrapper c s) = (colorName c, area s)

tagValue :: Tag -> Int
tagValue (Tag n) = n
tagValue Color = -1

main :: IO ()
main = do
  print ("colors", map colorName [Red, Green, Blue])
  print ("next", map (colorName . nextColor) [Red, Green, Blue])
  print ("next thrice", colorName (nextColor (nextColor (nextColor Red))))
  print ("area circle", area (Circle 2))
  print ("area rect", area (Rect 3 4))
  print ("area dot", area Dot)
  print ("corners", map corners [Circle 1, Rect 1 1, Dot])
  print ("single", singleName Single)
  print ("wrapper", wrapperInfo (Wrapper Blue (Rect 2 5)))
  print ("tag", tagValue (Tag 7), tagValue Color)
