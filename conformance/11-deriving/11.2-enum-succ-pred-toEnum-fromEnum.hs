-- Haskell 2010 Report 11.2: derived Enum for an enumeration: constructors are numbered left to right from 0;
-- succ, pred, toEnum, fromEnum
module Main (main) where

data Color = Red | Orange | Yellow | Green | Blue | Indigo | Violet deriving (Eq, Ord, Show, Enum)

-- Enum alone can be derived: no Eq, Ord or Show needed
data Plain = PA | PB | PC deriving (Enum)

colors :: [Color]
colors = [Red, Orange, Yellow, Green, Blue, Indigo, Violet]

plainName :: Plain -> String
plainName PA = "PA"
plainName PB = "PB"
plainName PC = "PC"

main :: IO ()
main = do
  print ("fromEnum", map fromEnum colors)
  print ("fromEnum first/last", fromEnum Red, fromEnum Violet)
  print ("toEnum", map toEnum [0 .. 6] :: [Color])
  print ("toEnum single", toEnum 0 :: Color, toEnum 3 :: Color, toEnum 6 :: Color)
  print ("toEnum . fromEnum", map (toEnum . fromEnum) colors == colors)
  print ("fromEnum . toEnum", map (\n -> fromEnum (toEnum n :: Color)) [0 .. 6] == [0 .. 6])
  print ("succ", map succ (init colors))
  print ("pred", map pred (tail colors))
  print ("succ single", succ Red, succ Indigo)
  print ("pred single", pred Violet, pred Orange)
  print ("succ . pred", map (succ . pred) (tail colors) == tail colors, map (pred . succ) (init colors) == init colors)
  print ("succ = toEnum . (+1) . fromEnum", all (\c -> succ c == toEnum (fromEnum c + 1)) (init colors))
  print ("iterate succ", take 7 (iterate succ Red), take 3 (iterate pred Violet))
  print ("order agrees with numbering", and [compare a b == compare (fromEnum a) (fromEnum b) | a <- colors, b <- colors])
  print ("Enum without Eq/Show", map fromEnum [PA, PB, PC], plainName (succ PA), plainName (pred PC), plainName (toEnum 1))
  print ("Enum without Eq/Show ranges", map plainName [PA ..], map plainName [PC, PB ..], map fromEnum (enumFromTo PB PC))
  print ("asTypeOf", toEnum 4 `asTypeOf` Red, fromEnum (last colors))
