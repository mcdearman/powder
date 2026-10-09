-- Haskell 2010 Report 6.3.4: the arithmetic sequence [a .. b] is enumFromTo and needs an Enum instance; this type has none
data Colour = Red | Green | Blue deriving (Eq, Show)

main :: IO ()
main = print [Red .. Blue]
