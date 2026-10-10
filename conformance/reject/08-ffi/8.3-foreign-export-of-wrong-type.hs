-- Haskell 2010 Report 8.3: the type of a foreign export is one the function has; here the function is
-- of Bool, and is exported at CInt
import Foreign.C.Types

foreign export ccall "powder_conformance_bad" bad :: CInt -> CInt

bad :: Bool -> Bool
bad = not

main :: IO ()
main = return ()
