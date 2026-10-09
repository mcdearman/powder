-- Haskell 2010 Report 8.4 / 8.7: foreign import ccall of integer C functions (abs, labs, llabs) and <ctype.h> functions at C integer types
import Foreign.C.Types

foreign import ccall "stdlib.h abs" c_abs :: CInt -> CInt

foreign import ccall "stdlib.h labs" c_labs :: CLong -> CLong

foreign import ccall "stdlib.h llabs" c_llabs :: CLLong -> CLLong

foreign import ccall "ctype.h toupper" c_toupper :: CInt -> CInt

foreign import ccall "ctype.h tolower" c_tolower :: CInt -> CInt

foreign import ccall "ctype.h isdigit" c_isdigit :: CInt -> CInt

foreign import ccall "ctype.h isalpha" c_isalpha :: CInt -> CInt

foreign import ccall "ctype.h isspace" c_isspace :: CInt -> CInt

viaC :: (CInt -> CInt) -> Char -> Char
viaC g = toEnum . fromIntegral . g . fromIntegral . fromEnum

test :: (CInt -> CInt) -> Char -> Bool
test g c = g (fromIntegral (fromEnum c)) /= 0

int :: CInt -> Int
int = fromIntegral

main :: IO ()
main = do
  print ("abs", int (c_abs (-5)), int (c_abs 5), int (c_abs 0), int (c_abs (-2147483647)))
  print ("labs", fromIntegral (c_labs (-123456)) :: Integer, fromIntegral (c_labs 99) :: Integer)
  print ("llabs", fromIntegral (c_llabs (-9000000000000000000)) :: Integer, fromIntegral (c_llabs 7) :: Integer)
  print ("toupper code", int (c_toupper 97), int (c_toupper 65), int (c_toupper 48))
  print ("tolower code", int (c_tolower 65), int (c_tolower 97), int (c_tolower 33))
  print ("toupper string", map (viaC c_toupper) "Hello, World 42!")
  print ("tolower string", map (viaC c_tolower) "Hello, World 42!")
  print ("isdigit", map (test c_isdigit) "a1 9Z0")
  print ("isalpha", map (test c_isalpha) "a1 9Z0")
  print ("isspace", map (test c_isspace) "a \t\nb")
  print ("digits only", filter (test c_isdigit) "tel: 555-0199 ext 7")
  print ("results are ordinary numbers", int (c_abs (-3) + c_abs (-4)), c_abs (-3) == 3, c_abs (-3) < c_abs (-4))
  print ("show at the C type", c_abs (-42), c_labs (-42), c_toupper 97)
  print ("argument computed in Haskell", int (c_abs (fromIntegral (negate (length "four")))), int (c_abs (10 - 25)))
  print ("round trip", map (viaC c_tolower . viaC c_toupper) "mixed CASE" == map (viaC c_tolower) "mixed CASE")
