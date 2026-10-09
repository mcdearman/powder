-- Haskell 2010 Report 6.3.1: (==) :: Eq a => a -> a -> Bool requires both operands to have the same type
main :: IO ()
main = print ((1 :: Int) == 'a')
