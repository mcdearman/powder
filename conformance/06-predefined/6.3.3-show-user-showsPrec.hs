-- Haskell 2010 Report 6.3.3: user Show instances; defining showsPrec (with precedences) or only show
data Expr = Lit Int | Add Expr Expr | Mul Expr Expr | Neg Expr

instance Show Expr where
  showsPrec _ (Lit n) = shows n
  showsPrec d (Add a b) = showParen (d > 6) (showsPrec 6 a . showString " + " . showsPrec 7 b)
  showsPrec d (Mul a b) = showParen (d > 7) (showsPrec 7 a . showString " * " . showsPrec 8 b)
  showsPrec d (Neg e) = showParen (d > 9) (showChar '-' . showsPrec 10 e)

-- only show is defined: the default showsPrec ignores the precedence
newtype Token = Token String

instance Show Token where
  show (Token s) = "<" ++ s ++ ">"

-- a parameterised type with a context
data Pair a = Pair a a

instance Show a => Show (Pair a) where
  showsPrec d (Pair x y) = showParen (d > 10) (showString "Pair " . showsPrec 11 x . showChar ' ' . showsPrec 11 y)

one, two, three :: Expr
one = Lit 1
two = Lit 2
three = Lit 3

main :: IO ()
main = do
  print (Add one (Mul two three))
  print (Mul (Add one two) three)
  print (Add (Add one two) three)
  print (Add one (Add two three))
  print (Mul one (Mul two three))
  print (Neg (Add one two))
  print (Add (Neg one) two)
  print (Neg (Neg one))
  print (Just (Add one two))
  print (Just one)
  print [Add one two, three]
  print (Add one two, Mul two three)
  putStrLn (showsPrec 7 (Add one two) "")
  putStrLn (showsPrec 6 (Add one two) "")
  putStrLn (shows (Mul one two) " = 2")
  print (Token "a")
  print (Just (Token "a"))
  print [Token "a", Token "b"]
  putStrLn (showsPrec 11 (Token "p") "")
  putStrLn (shows (Token "q") "!")
  print (Pair 'a' 'b')
  print (Pair (-1) (2 :: Int))
  print (Pair (Just True) Nothing)
  print (Just (Pair one two))
  print (Pair (Pair (Token "x") (Token "y")) (Pair (Token "z") (Token "w")))
