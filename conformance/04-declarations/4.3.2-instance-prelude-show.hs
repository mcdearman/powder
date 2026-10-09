-- Haskell 2010 Report 4.3.2: user Show instances via showsPrec, via show only, and with a customised showList
module Main (main) where

infixl 6 :+:
infixl 7 :*:

data Expr = Lit Int | Expr :+: Expr | Expr :*: Expr | Neg Expr

instance Show Expr where
  showsPrec _ (Lit n) = shows n
  showsPrec d (a :+: b) = showParen (d > 6) (showsPrec 6 a . showString " + " . showsPrec 7 b)
  showsPrec d (a :*: b) = showParen (d > 7) (showsPrec 7 a . showString " * " . showsPrec 8 b)
  showsPrec d (Neg e) = showParen (d > 10) (showString "Neg " . showsPrec 11 e)

data Bit = O | I

instance Show Bit where
  showsPrec _ O = showChar '0'
  showsPrec _ I = showChar '1'
  showList bs rest = "0b" ++ concatMap show bs ++ rest

-- only `show` is defined; showsPrec and showList come from the class defaults
data Unit = Unit

instance Show Unit where
  show Unit = "U!"

newtype Temp = Temp Int

instance Show Temp where
  showsPrec d (Temp n) = showParen (d > 10) (showString "Temp " . showsPrec 11 n)

main :: IO ()
main = do
  print ("precedence", show (Lit 1 :+: Lit 2 :*: Lit 3))
  print ("parens needed", show ((Lit 1 :+: Lit 2) :*: Lit 3))
  print ("left nested", show (Lit 1 :+: Lit 2 :+: Lit 3))
  print ("right nested", show (Lit 1 :+: (Lit 2 :+: Lit 3)))
  print ("neg", show (Neg (Lit 1 :+: Lit 2)), show (Neg (Lit 5)))
  print ("in Just", show (Just (Lit 1 :+: Lit 2)), show (Just (Lit 5)))
  print ("in list", show [Lit 1, Neg (Lit 2), Lit 3 :*: Lit 4])
  print ("in tuple", show (Lit 1 :+: Lit 2, Lit 3))
  print ("shows appends", shows (Lit 1 :*: Lit 2) "!")
  print ("showsPrec 7", showsPrec 7 (Lit 1 :+: Lit 2) "")
  print ("bit", show O, show I)
  print ("bit list", show [I, O, I])
  print ("bit empty list", show ([] :: [Bit]))
  print ("bit list in Just", show (Just [I]))
  print ("bit nested lists", show [[I], [O, O]])
  print ("bit in tuple", show (I, [O]))
  print ("showList directly", showList [I, I] "!")
  print ("unit", show Unit)
  print ("unit showsPrec default", showsPrec 11 Unit "?")
  print ("unit in Just", show (Just Unit))
  print ("unit list default showList", show [Unit, Unit])
  print ("temp", show (Temp 3), show (Temp (-3)))
  print ("temp in Just", show (Just (Temp 3)), show [Temp 1])
