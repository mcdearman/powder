-- Haskell 2010 Report 8.5.1: a "wrapper" import makes a C function of a function here, which C then
-- calls: qsort and bsearch with comparisons written here
module Main (main) where

import Foreign
import Foreign.C

type Compare = Ptr CInt -> Ptr CInt -> IO CInt

foreign import ccall "wrapper" mkCompare :: Compare -> IO (FunPtr Compare)
foreign import ccall "stdlib.h qsort" c_qsort :: Ptr CInt -> CSize -> CSize -> FunPtr Compare -> IO ()
foreign import ccall "stdlib.h bsearch" c_bsearch :: Ptr CInt -> Ptr CInt -> CSize -> CSize -> FunPtr Compare -> IO (Ptr CInt)
foreign import ccall "wrapper" mkUnary :: (CDouble -> CDouble) -> IO (FunPtr (CDouble -> CDouble))
foreign import ccall "dynamic" callUnary :: FunPtr (CDouble -> CDouble) -> CDouble -> CDouble
foreign import ccall "wrapper" mkMixed :: (CInt -> CDouble -> CChar -> Bool -> IO CLong) -> IO (FunPtr (CInt -> CDouble -> CChar -> Bool -> IO CLong))
foreign import ccall "dynamic" callMixed :: FunPtr (CInt -> CDouble -> CChar -> Bool -> IO CLong) -> CInt -> CDouble -> CChar -> Bool -> IO CLong
foreign import ccall "wrapper" mkAction :: IO () -> IO (FunPtr (IO ()))
foreign import ccall "dynamic" callAction :: FunPtr (IO ()) -> IO ()

by :: (CInt -> CInt -> Ordering) -> Compare
by order a b = do
  x <- peek a
  y <- peek b
  return (fromIntegral (fromEnum (order x y)) - 1)

sortWith :: FunPtr Compare -> [CInt] -> IO [CInt]
sortWith cmp xs = withArrayLen xs (\n p -> c_qsort p (fromIntegral n) 4 cmp >> peekArray n p)

main :: IO ()
main = do
  up <- mkCompare (by compare)
  down <- mkCompare (by (flip compare))
  a <- sortWith up [5, 3, 9, -1, 4, 0, 3]
  b <- sortWith down [5, 3, 9, -1, 4, 0, 3]
  print ("qsort calls back", a, b)
  byLastDigit <- mkCompare (by (\x y -> compare (x `mod` 10) (y `mod` 10)))
  c <- sortWith byLastDigit [31, 12, 5, 40, 23]
  print ("a comparison that closes over nothing but its definition", c)
  found <- withArrayLen a $ \n p -> with 4 $ \key -> do
    hit <- c_bsearch key p (fromIntegral n) 4 up
    miss <- with 7 (\other -> c_bsearch other p (fromIntegral n) 4 up)
    return (hit `minusPtr` p, miss == nullPtr)
  print ("bsearch calls back", found)
  half <- mkUnary (/ 2)
  print ("a function that is not an action", callUnary half 5, callUnary half (callUnary half 1))
  let offset = 100 :: CLong
  mixed <- mkMixed (\i d ch flag -> return (offset + fromIntegral i + truncate d + fromIntegral ch + (if flag then 1000 else 0)))
  m1 <- callMixed mixed 1 2.5 3 True
  m2 <- callMixed mixed (-1) (-2.5) (-3) False
  print ("arguments of four types, and a closure", m1, m2)
  act <- mkAction (putStrLn "an action with no arguments runs when C calls it")
  callAction act
  callAction act
  nested <- mkUnary (\x -> callUnary half x + 1)
  print ("a call back that itself calls C that calls back", callUnary nested 9)
  print ("the functions are not null, and differ", up /= nullFunPtr, up /= down)
  mapM_ freeHaskellFunPtr [up, down, byLastDigit]
  freeHaskellFunPtr half
  putStrLn "freed"
