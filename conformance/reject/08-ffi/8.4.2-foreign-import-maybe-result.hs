-- Haskell 2010 Report 8.4.2: the result type of a foreign import must be a marshallable foreign type (or IO of one); Maybe Int is not
foreign import ccall "stdlib.h abs" c_abs :: Int -> Maybe Int

main :: IO ()
main = print (c_abs 1)
