-- Haskell 2010 Report 11.4: an operator-named constructor DECLARED in prefix form, `data T = (:*:) Int Int`, is shown
-- in prefix form with the operator in parentheses, as an ordinary application (parenthesised when d > 10)
module Main (main) where

infixl 6 :*:

data T = (:*:) Int Int deriving (Show)

-- the same operator-style name declared infix in another type, for contrast
data S = Int :+: Int deriving (Show)

infixl 6 :+:

-- prefix-declared operator constructors with other arities
data Z = (:@) deriving (Show)

data One = (:!) Int deriving (Show)

data Three = (:#) Int Char Bool deriving (Show)

-- recursive and mixed
data L = Nil | (:>) Int L deriving (Show)

infixr 5 :>

main :: IO ()
main = do
  print ((:*:) 1 2)
  print (1 :*: 2)
  print ((-1) :*: (-2))
  print (Just (1 :*: 2))
  print [1 :*: 2, 3 :*: 4]
  putStrLn (showsPrec 6 (1 :*: 2) "" ++ " | " ++ showsPrec 10 (1 :*: 2) "" ++ " | " ++ showsPrec 11 (1 :*: 2) "")
  print (1 :+: 2)
  print ((:+:) 1 2)
  print (Just ((-1) :+: 2))
  putStrLn (showsPrec 6 (1 :+: 2) "" ++ " | " ++ showsPrec 7 (1 :+: 2) "" ++ " | " ++ showsPrec 11 (1 :+: 2) "")
  print (:@)
  print (Just (:@), [(:@), (:@)])
  putStrLn (showsPrec 11 (:@) "")
  print ((:!) 5)
  print ((:!) (-5), Just ((:!) 5))
  print ((:#) 1 'c' True)
  print (Just ((:#) (-1) 'c' False))
  print Nil
  print (1 :> Nil)
  print (1 :> 2 :> 3 :> Nil)
  print ((:>) (-1) Nil)
  print (Just (1 :> Nil), [Nil, 2 :> Nil])
