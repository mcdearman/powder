-- Haskell 2010 Report 11.4: a constructor DECLARED in infix form with a backquoted name is shown infix with backquotes;
-- a constructor declared prefix is shown prefix even if it is used infix in the program
module Main (main) where

infixr 5 `Foo`

infix 4 `Then`

-- declared infix, with a fixity declaration
data T = Int `Foo` Int deriving (Show)

-- declared infix, default fixity infixl 9
data U = Int `Baz` Int deriving (Show)

-- declared prefix (the fixity declaration only affects how expressions are parsed)
data V = Then Int Int deriving (Show)

-- recursive, declared infix
data Chain = End | Int `Link` Chain deriving (Show)

infixr 5 `Link`

-- parameterised, declared infix
data Two a b = a `With` b deriving (Show)

infixl 6 `With`

main :: IO ()
main = do
  print (1 `Foo` 2)
  print (Foo 1 2)
  print ((-1) `Foo` (-2))
  print (Just (1 `Foo` 2), [1 `Foo` 2, 3 `Foo` 4])
  putStrLn (showsPrec 5 (1 `Foo` 2) "" ++ " | " ++ showsPrec 6 (1 `Foo` 2) "" ++ " | " ++ showsPrec 11 (1 `Foo` 2) "")
  print (1 `Baz` 2)
  print (Baz 1 2)
  print ((-1) `Baz` (-2))
  print (Just (1 `Baz` 2))
  putStrLn (showsPrec 9 (1 `Baz` 2) "" ++ " | " ++ showsPrec 10 (1 `Baz` 2) "" ++ " | " ++ showsPrec 11 (1 `Baz` 2) "")
  print (1 `Then` 2)
  print (Then 1 2)
  print ((-1) `Then` (-2))
  print (Just (1 `Then` 2))
  putStrLn (showsPrec 4 (1 `Then` 2) "" ++ " | " ++ showsPrec 10 (1 `Then` 2) "" ++ " | " ++ showsPrec 11 (1 `Then` 2) "")
  print End
  print (1 `Link` End)
  print (1 `Link` 2 `Link` 3 `Link` End)
  print (Link 1 (Link (-2) End))
  print (Just (1 `Link` End))
  print ('a' `With` True)
  print ((1 :: Int) `With` 'b' `With` "c")
  print (Just 'x' `With` [1, 2 :: Int])
  print (('a' `With` 'b') `With` ('c' `With` 'd'))
  print ('a' `With` ('b' `With` 'c'))
  print ((-1 :: Int) `With` (-2.5 :: Double))
