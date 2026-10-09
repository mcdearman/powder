-- Haskell 2010 Report 4.4.3.2: a pattern binding's pattern and right side must have one type; a list is not
-- a Bool, and a Bool is not a unit
() = [] || []

main :: IO ()
main = return ()
