-- Haskell 2010 Report 11.2: for a derived Enum,
--   enumFrom x       = enumFromTo x lastCon
--   enumFromThen x y = enumFromThenTo x y bound  where bound | fromEnum y >= fromEnum x = lastCon
--                                                            | otherwise                = firstCon
-- checked exhaustively on a five-constructor enumeration
module Main (main) where

data Level = L0 | L1 | L2 | L3 | L4 deriving (Eq, Show, Enum)

firstCon :: Level
firstCon = L0

lastCon :: Level
lastCon = L4

levels :: [Level]
levels = [L0, L1, L2, L3, L4]

boundFor :: Level -> Level -> Level
boundFor x y
  | fromEnum y >= fromEnum x = lastCon
  | otherwise = firstCon

-- the Report's default-method view of the sequences, via Int
viaInt :: Level -> Level -> Level -> [Level]
viaInt x y z = map toEnum [fromEnum x, fromEnum y .. fromEnum z]

main :: IO ()
main = do
  print ("enumFrom = enumFromTo x lastCon", and [enumFrom x == enumFromTo x lastCon | x <- levels])
  print ("enumFromThen = enumFromThenTo x y bound", and [enumFromThen x y == enumFromThenTo x y (boundFor x y) | x <- levels, y <- levels, x /= y])
  print ("enumFromTo via Int", and [enumFromTo x z == map toEnum [fromEnum x .. fromEnum z] | x <- levels, z <- levels])
  print ("enumFromThenTo via Int", and [enumFromThenTo x y z == viaInt x y z | x <- levels, y <- levels, z <- levels, x /= y])
  mapM_ (\x -> print ("enumFrom", x, enumFrom x)) levels
  mapM_ (\(x, y) -> print ("enumFromThen", x, y, enumFromThen x y)) [(x, y) | x <- levels, y <- levels, x /= y]
  print ("ascending ends at lastCon", last [L1, L2 ..], last [L0, L2 ..], last [L0, L3 ..], last [L0, L4 ..])
  print ("descending ends at firstCon", last [L3, L2 ..], last [L4, L2 ..], last [L4, L1 ..], last [L4, L0 ..])
  print ("equal x y: bound is lastCon, list is infinite", take 3 (enumFromThen L2 L2), take 3 (enumFromThen L4 L4), take 3 (enumFromThen L0 L0))
  print ("lengths of enumFrom", map (length . enumFrom) levels)
  print ("lengths of enumFromThen up", [length (enumFromThen L0 y) | y <- tail levels])
  print ("lengths of enumFromThen down", [length (enumFromThen L4 y) | y <- init levels])
