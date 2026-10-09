-- Haskell 2010 Report 11.4: derived Read for nullary and prefix constructors: arbitrary white space between lexemes,
-- redundant parentheses around the value and around arguments, arguments read at precedence 11
module Main (main) where

data Color = Red | Green | Blue deriving (Eq, Show, Read)

data T = N | U Int | B Int Int | W T | V T T | M (Maybe T) deriving (Eq, Show, Read)

-- constructor names that are prefixes of one another
data X = Ab | Abc | A' | A'b deriving (Eq, Show, Read)

main :: IO ()
main = do
  print ("nullary", read "Red" :: Color, read "Green" :: Color, read "Blue" :: Color)
  print ("nullary white space", read "  Red" :: Color, read "Red  " :: Color, read "\n\tRed\n" :: Color)
  print ("nullary parentheses", read "(Red)" :: Color, read "((Red))" :: Color, read "  ( A' )  " :: X, read " ( ( Blue ) ) " :: Color)
  print ("names with common prefixes", read "Ab" :: X, read "Abc" :: X, read "A'" :: X, read "A'b" :: X)
  print ("one argument", read "U 1" :: T, read "U 0" :: T, read "U 123456789" :: T)
  print ("two arguments", read "B 1 2" :: T, read "B 10 20" :: T)
  print ("white space", read "  B   1   2  " :: T, read "B\n1\t2" :: T, read "U\t7" :: T)
  print ("parentheses around the value", read "(B 1 2)" :: T, read "((U 1))" :: T, read " ( B 1 2 ) " :: T)
  print ("parentheses around arguments", read "B (1) 2" :: T, read "B (1) (2)" :: T, read "U ((1))" :: T, read "B(1)(2)" :: T)
  print ("negative arguments", read "U (-1)" :: T, read "B (-1) (-2)" :: T, read "U ( -3 )" :: T)
  print ("nested", read "W N" :: T, read "W (U 1)" :: T, read "W (W (W N))" :: T)
  print ("nested two", read "V N N" :: T, read "V (U 1) (B 2 3)" :: T, read "V (V N N) (W N)" :: T)
  print ("nested parentheses", read "W (N)" :: T, read "(W ((U (1))))" :: T, read "V(N)(N)" :: T)
  print ("Maybe field", read "M Nothing" :: T, read "M (Just N)" :: T, read "M (Just (U (-1)))" :: T, read "M (Just (M Nothing))" :: T)
  print ("in Prelude structures", read "Just Red" :: Maybe Color, read "Just (U 1)" :: Maybe T, read "(Red,U 1)" :: (Color, T))
  print ("in lists", read "[Red,Green]" :: [Color], read "[U 1,N,B 2 3]" :: [T], read "[W (U 1)]" :: [T])
  print ("in Either", read "Left Red" :: Either Color T, read "Right (B 1 2)" :: Either Color T)
  print ("equal to constructed", (read "V (U 1) (M (Just N))" :: T) == V (U 1) (M (Just N)), (read "Abc" :: X) == Abc)
  print ("all colours", map read ["Red", "Green", "Blue"] :: [Color])
  print ("read . show", all (\c -> read (show c) == c) [Red, Green, Blue], all (\t -> read (show t) == t) [N, U (-5), B 1 (-2), W (W N), V (U 1) N, M (Just (M Nothing))])
