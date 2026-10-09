-- Haskell 2010 Report 3.4: -e means Prelude.negate e (even when negate or (-) is rebound locally); -1 is negate (fromInteger 1)
module Main (main) where

data T = Lit Integer | Neg T | Sub T T | Other
  deriving (Eq, Show)

instance Num T where
  fromInteger n = Lit n
  negate t = Neg t
  a - b = Sub a b
  _ + _ = Other
  _ * _ = Other
  abs _ = Other
  signum _ = Other

seven :: T
seven = Lit 7

main :: IO ()
main = do
  print ("-1", (-1) :: T)
  print ("- x", - seven)
  print ("1 - 2", (1 - 2) :: T)
  print ("- 1 - 2", (- 1 - 2) :: T)
  print ("- (1 - 2)", (- (1 - 2)) :: T)
  print ("- - via parens", (- (- 1)) :: T)
  print ("subtract", subtract 1 2 :: T)
  print ("negate literal", negate 3 :: T)
  -- a local binding named negate does not change the meaning of prefix minus
  print ("local negate", let negate n = n + 100 in (- (5 :: Int), negate (5 :: Int)))
  -- binary minus is an ordinary operator and can be rebound; prefix minus is unaffected
  print ("local minus", let a - b = a * b in (7 - (3 :: Int), - (3 :: Int)))
