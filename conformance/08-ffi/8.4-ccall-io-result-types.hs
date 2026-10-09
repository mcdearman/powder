-- Haskell 2010 Report 8.4.1 / 8.5.1: a foreign import may have result type IO t; the call is then an I/O action
import Foreign.C.Types

foreign import ccall "math.h sqrt" io_sqrt :: CDouble -> IO CDouble

foreign import ccall "math.h pow" io_pow :: CDouble -> CDouble -> IO CDouble

foreign import ccall "math.h fma" io_fma :: CDouble -> CDouble -> CDouble -> IO CDouble

foreign import ccall "stdlib.h abs" io_abs :: CInt -> IO CInt

foreign import ccall "ctype.h toupper" io_toupper :: CInt -> IO CInt

foreign import ccall "math.h sqrt" pure_sqrt :: CDouble -> CDouble

toD :: CDouble -> Double
toD = realToFrac

hypotenuse :: CDouble -> CDouble -> IO CDouble
hypotenuse a b = do
  a2 <- io_pow a 2
  b2 <- io_pow b 2
  io_sqrt (a2 + b2)

main :: IO ()
main = do
  r <- io_sqrt 16
  print ("sqrt in IO", toD r)
  p <- io_pow 2 10
  print ("pow in IO", toD p)
  m <- io_fma 2 3 4
  print ("fma in IO", toD m)
  a <- io_abs (-5)
  print ("abs in IO", fromIntegral a :: Int)
  u <- io_toupper 97
  print ("toupper in IO", toEnum (fromIntegral u) :: Char)
  h <- hypotenuse 3 4
  print ("sequenced calls", toD h)
  rs <- mapM io_sqrt [1, 4, 9, 16]
  print ("mapM", map toD rs)
  s <- io_sqrt 81 >>= io_sqrt
  print ("bind chain", toD s)
  t <- fmap toD (io_pow 3 3)
  print ("fmap", t)
  let action = io_sqrt 49
  x <- action
  y <- action
  print ("an action can be run twice", toD x, toD y)
  print ("same C function imported pure and in IO", toD (pure_sqrt 16) == toD r)
  us <- mapM (io_toupper . fromIntegral . fromEnum) "shout"
  putStrLn (map (toEnum . fromIntegral) us)
