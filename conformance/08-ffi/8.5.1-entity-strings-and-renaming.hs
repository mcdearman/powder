-- Haskell 2010 Report 8.5.1: ccall import entity strings: "header.h cid", "cid", "static cid", "static header.h cid",
-- and an omitted entity (the C name is then the Haskell name); a C function may be imported under any Haskell name
import Foreign.C.Types

-- header and C identifier
foreign import ccall "math.h fabs" withHeader :: CDouble -> CDouble

-- C identifier only
foreign import ccall "fabs" plainName :: CDouble -> CDouble

-- explicit static, with and without header
foreign import ccall "static fabs" staticName :: CDouble -> CDouble

foreign import ccall "static math.h fabs" staticWithHeader :: CDouble -> CDouble

-- extra white space between the parts of the entity string
foreign import ccall "static   math.h   fabs" spacedEntity :: CDouble -> CDouble

-- no entity string: the Haskell identifier is the C identifier
foreign import ccall fabs :: CDouble -> CDouble

foreign import ccall fmod :: CDouble -> CDouble -> CDouble

foreign import ccall unsafe labs :: CLong -> CLong

foreign import ccall safe toupper :: CInt -> CInt

-- renaming: Haskell names unrelated to the C names
foreign import ccall "abs" absoluteValue :: CInt -> CInt

foreign import ccall "pow" raisedTo :: CDouble -> CDouble -> CDouble

foreign import ccall "ceil" roundUp' :: CDouble -> CDouble

-- a Haskell name that is also a different C function's name: the entity string decides
foreign import ccall "floor" ceil :: CDouble -> CDouble

toD :: CDouble -> Double
toD = realToFrac

main :: IO ()
main = do
  print ("header and name", toD (withHeader (-1.5)))
  print ("name only", toD (plainName (-2.5)))
  print ("static name", toD (staticName (-3.5)))
  print ("static header name", toD (staticWithHeader (-4.5)))
  print ("extra spaces", toD (spacedEntity (-5.5)))
  print ("omitted entity fabs", toD (fabs (-6.5)))
  print ("omitted entity fmod", toD (fmod 10 4))
  print ("omitted entity labs", fromIntegral (labs (-77)) :: Int)
  print ("omitted entity toupper", toEnum (fromIntegral (toupper 122)) :: Char)
  print ("renamed abs", fromIntegral (absoluteValue (-9)) :: Int)
  print ("renamed pow as an operator", toD (2 `raisedTo` 5))
  print ("renamed ceil with a prime", toD (roundUp' 2.25))
  print ("Haskell name ceil bound to C floor", toD (ceil 2.75))
  print ("all fabs imports agree", and [f x == fabs x | f <- [withHeader, plainName, staticName, staticWithHeader, spacedEntity], x <- [-2, -0.5, 0, 3]])
