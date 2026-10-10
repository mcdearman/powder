-- Haskell 2010 Report 29 (Foreign.Marshal): unsafeLocalState, an action that only works on memory of its own
-- as the value it gives. GHC's base no longer has it, so GHC does not compile this.
module Main (main) where

import Foreign.Marshal
import Foreign.Storable

main :: IO ()
main = print ("unsafeLocalState", unsafeLocalState (alloca (\p -> poke p (20 :: Int) >> peek p >>= \x -> return (x + 1))))
