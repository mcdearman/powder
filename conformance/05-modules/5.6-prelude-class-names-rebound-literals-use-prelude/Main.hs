-- Haskell 2010 Report 5.6.2, 6.4.1, 3.2: a module that does not import the Prelude's Num may define a class
-- of that name with those methods; a numeric literal still means the Prelude's fromInteger
module Main (main) where

import Prelude hiding (Num (..))
import qualified Prelude as P

infixl 6 +, -

infixl 7 *

-- The Prelude's Num has Eq and Show above it.
data Zero = Zero deriving (Eq, Show)

class Num a where
  (+), (-), (*) :: a -> a -> a
  negate :: a -> a
  fromInteger :: Integer -> a

instance Num Zero where
  Zero + Zero = Zero
  Zero - Zero = Zero
  Zero * Zero = Zero
  negate Zero = Zero
  fromInteger _ = Zero

instance Num Integer where
  (+) = (P.+)
  (-) = (P.-)
  (*) = (P.*)
  negate = P.negate
  fromInteger = id

instance P.Num Zero where
  Zero + Zero = Zero
  Zero - Zero = Zero
  Zero * Zero = Zero
  negate Zero = Zero
  abs Zero = Zero
  signum Zero = Zero
  fromInteger _ = Zero

zeros :: [Zero]
zeros = [1, 2, 3]

main :: IO ()
main = do
  print zeros
  print (Zero + Zero * Zero - negate Zero)
  print (fromInteger 5 :: Zero, P.fromInteger 5 :: Zero)
  print (2 + 3 * 4 - 1 :: Integer, 2 P.+ 3 P.* 4 :: Int)
  print (negate (7 :: Integer), fromInteger 9 + (1 :: Integer))
