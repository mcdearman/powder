-- Haskell 2010 Report 5.2 / 4.3: class in one module, subclass in a second, types and instances in others; superclass
-- methods are usable through a subclass constraint without importing the superclass by name
module Main (main) where

import Sub (Sub(..), viaSub)
import Types (A(..), B(..))
import qualified Super

newtype Local = Local Int

instance Super.Super Local where
  base (Local n) = n

instance Sub Local where
  extra (Local n) = "local" ++ show n

both :: Sub a => a -> (Int, String)
both x = (Super.base x, extra x)

main :: IO ()
main = do
  print ("subclass method", extra A, extra (B 2))
  print ("superclass method via qualified import", Super.base A, Super.base (B 2))
  print ("superclass default method", Super.twiceBase (B 5), Super.twiceBase A)
  print ("function needing only the subclass", viaSub A, viaSub (B 3))
  print ("local instance of both classes", both (Local 7), viaSub (Local 7))
  print ("instance on list from Sub", extra [B 1, B 2], Super.base [B 3, B 4])
