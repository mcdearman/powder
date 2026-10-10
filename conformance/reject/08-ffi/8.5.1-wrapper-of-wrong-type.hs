-- Haskell 2010 Report 8.5.1: a "wrapper" import has the type ft -> IO (FunPtr ft); here the two function
-- types differ
import Foreign.C.Types
import Foreign.Ptr

foreign import ccall "wrapper" mk :: (CInt -> CInt) -> IO (FunPtr (CInt -> CDouble))

main :: IO ()
main = return ()
