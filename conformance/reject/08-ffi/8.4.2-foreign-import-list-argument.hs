-- Haskell 2010 Report 8.4.2: argument types of a foreign import must be marshallable foreign types; [Int] is not
foreign import ccall "stdlib.h abs" c_abs :: [Int] -> Int

main :: IO ()
main = print (c_abs [1])
