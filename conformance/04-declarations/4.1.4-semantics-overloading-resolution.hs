-- Haskell 2010 Report 4.1.4: overloading resolved from the context of use, including on the result type
module Main (main) where

class Default a where
  def :: a

instance Default Int where
  def = 0

instance Default Bool where
  def = False

instance Default Char where
  def = '?'

instance Default a => Default [a] where
  def = [def]

instance (Default a, Default b) => Default (a, b) where
  def = (def, def)

instance Default a => Default (Maybe a) where
  def = Just def

class Parse a where
  parse :: String -> a

instance Parse Int where
  parse = length

instance Parse Bool where
  parse s = s == "yes"

needsInt :: Int -> Int
needsInt x = x + 100

main :: IO ()
main = do
  print ("def int", def :: Int)
  print ("def bool", def :: Bool)
  print ("def string", def :: String)
  print ("def nested", def :: [(Int, Maybe Bool)])
  print ("def from not", not def)
  print ("def from argument", needsInt def)
  print ("def from arithmetic", def + (1 :: Int))
  print ("def from list", [def, 5 :: Int])
  print ("def from pair", fst (def :: (Char, Int)))
  print ("parse int", parse "four" :: Int)
  print ("parse bool", parse "yes" :: Bool)
  print ("parse both", if parse "yes" then needsInt (parse "abc") else 0)
  print ("minBound", minBound :: Int, minBound :: Char, minBound :: Bool)
  print ("maxBound", maxBound :: Int, maxBound :: Bool)
  print ("read", read "42" :: Int, read "[1,2]" :: [Int])
  print ("fromInteger", fromInteger 7 :: Double, fromInteger 7 :: Int)
  print ("toEnum", toEnum 65 :: Char, toEnum 1 :: Bool)
