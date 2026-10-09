-- Haskell 2010 Report 8.4.2: a function type is not a marshallable argument type of a (static) foreign import
foreign import ccall "stdlib.h abs" c_apply :: (Int -> Int) -> Int

main :: IO ()
main = print (c_apply (+ 1))
