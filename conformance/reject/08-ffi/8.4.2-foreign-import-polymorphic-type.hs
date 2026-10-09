-- Haskell 2010 Report 8.4.2: a foreign type may not contain type variables; a -> a is not a foreign type
foreign import ccall "stdlib.h abs" c_id :: a -> a

main :: IO ()
main = print (c_id (1 :: Int))
