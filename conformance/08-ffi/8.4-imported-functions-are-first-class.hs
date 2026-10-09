-- Haskell 2010 Report 8.4: a foreign import declares an ordinary Haskell variable; it can be passed, stored, composed and partially applied
import Foreign.C.Types

foreign import ccall "math.h sqrt" c_sqrt :: CDouble -> CDouble

foreign import ccall "math.h fabs" c_fabs :: CDouble -> CDouble

foreign import ccall "math.h floor" c_floor :: CDouble -> CDouble

foreign import ccall "math.h pow" c_pow :: CDouble -> CDouble -> CDouble

foreign import ccall "math.h fma" c_fma :: CDouble -> CDouble -> CDouble -> CDouble

foreign import ccall "stdlib.h abs" c_abs :: CInt -> CInt

data Op = Op String (CDouble -> CDouble)

ops :: [Op]
ops = [Op "sqrt" c_sqrt, Op "fabs" c_fabs, Op "floor" c_floor, Op "square" (flip c_pow 2), Op "sqrt . fabs" (c_sqrt . c_fabs)]

applyTwice :: (a -> a) -> a -> a
applyTwice g = g . g

toD :: CDouble -> Double
toD = realToFrac

norm :: [CDouble] -> CDouble
norm = c_sqrt . sum . map square
  where
    square = flip c_pow 2

main :: IO ()
main = do
  mapM_ (\(Op name g) -> print (name, toD (g 16), toD (g (-2.25) * g (-2.25)))) (take 2 (drop 1 ops) ++ drop 3 (take 4 ops))
  print ("stored in a list", [toD (g 6.25) | Op _ g <- ops])
  print ("composition", toD ((c_sqrt . c_sqrt) 81), toD ((c_floor . c_sqrt . c_fabs) (-10)))
  print ("applyTwice", toD (applyTwice c_sqrt 256), fromIntegral (applyTwice c_abs (-5)) :: Int)
  print ("partial application", map (toD . c_pow 2) [1, 2, 3], map (toD . c_fma 2 3) [0, 1, 2], toD (c_fma 10 10 1))
  print ("sections", map (toD . (`c_pow` 2)) [1, 2, 3], map (toD . (2 `c_pow`)) [1, 2, 3])
  print ("folds", toD (foldr (\x acc -> c_fma x x acc) 0 [1, 2, 3]), toD (foldl c_pow 2 [2, 2]))
  print ("where-bound helper", toD (norm [3, 4]), toD (norm [1, 2, 2]))
  print ("let-bound alias", let root = c_sqrt in toD (root 49))
  print ("in a tuple and Maybe", toD (fst (c_sqrt, c_fabs) 4), fmap (toD . c_fabs) (Just (-8)))
  print ("lazy: an unused foreign call is never made", const 'k' (c_sqrt 2))
  print ("until", toD (until (<= 2) c_sqrt 65536))
  print ("iterate", map toD (take 4 (iterate c_sqrt 65536)))
