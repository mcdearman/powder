-- Haskell 2010 Report 4.4.3.1: infix forms of function bindings: x `op` y, x <+> y, (x ! y) z, and prefix (<+>) x y
module Main (main) where

infixl 6 `plus`
infixr 5 <+>

plus :: Int -> Int -> Int
x `plus` y = x + y

(<+>) :: [a] -> [a] -> [a]
[] <+> ys = ys
(x : xs) <+> ys = x : (xs <+> ys)

-- operator defined in prefix form
(|+|) :: Int -> Int -> Int
(|+|) a b = a + b

-- an infix left-hand side in parentheses, followed by more arguments
(!) :: [(String, Int)] -> String -> Int -> Int
(env ! key) dflt = maybe dflt id (lookup key env)

applyN :: (a -> a) -> Int -> a -> a
(f `applyN` n) x
  | n <= 0 = x
  | otherwise = (f `applyN` (n - 1)) (f x)

-- infix clauses with guards and literal patterns
cmp :: Int -> Int -> String
a `cmp` b
  | a < b = "lt"
  | a == b = "eq"
  | otherwise = "gt"

divides :: Int -> Int -> Bool
_ `divides` 0 = True
0 `divides` _ = False
d `divides` n = n `mod` d == 0

-- constructor patterns on both sides of an infix definition
data V = V Int Int deriving (Eq, Show)

(^+^) :: V -> V -> V
V a b ^+^ V c d = V (a + c) (b + d)

orElse :: Maybe a -> Maybe a -> Maybe a
Nothing `orElse` y = y
x@(Just _) `orElse` _ = x

main :: IO ()
main = do
  print ("backquoted definition", 1 `plus` 2 `plus` 3, plus 4 5)
  print ("operator definition", "ab" <+> "cd" <+> "", [1] <+> [2 :: Int])
  print ("operator used prefix", (<+>) "x" "y")
  print ("prefix definition", 1 |+| 2, (|+|) 3 4)
  print ("parenthesised lhs", ([("a", 1)] ! "a") 0, ([("a", 1)] ! "b") 0)
  print ("parenthesised lhs partial", map ([("x", 9)] ! "x") [1, 2])
  print ("backquoted parenthesised lhs", ((+ 2) `applyN` 3) 0, (('x' :) `applyN` 2) "")
  print ("infix with guards", 1 `cmp` 2, 2 `cmp` 2, 3 `cmp` 2)
  print ("infix with literals", 3 `divides` 0, 0 `divides` 3, 3 `divides` 9, 3 `divides` 10)
  print ("constructor patterns", V 1 2 ^+^ V 3 4)
  print ("as-pattern operand", Just 'a' `orElse` Just 'b', Nothing `orElse` Just 'b')
