-- Haskell 2010 Report 6.2: seq :: a -> b -> b returns its second argument, here a String where an Int is required
n :: Int
n = seq 'a' "text"

main :: IO ()
main = print n
