-- Haskell 2010 Report 4.1.3, 6.3.1: (==) needs Eq, which a signature without a context does not give
badEq :: a -> a -> Bool
badEq a b = a == b

main :: IO ()
main = print (badEq 'a' 'a')
