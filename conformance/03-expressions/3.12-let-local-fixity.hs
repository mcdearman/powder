-- Haskell 2010 Report 3.12 / 4.4.2: fixity declarations local to a let (or where) apply only within its scope
module Main (main) where

infixl 6 ><

(><) :: Int -> Int -> Int
a >< b = a - b

viaWhere :: Int
viaWhere = 2 <+> 3 <.> 4
  where
    infixl 6 <+>
    infixl 7 <.>
    a <+> b = a + b
    a <.> b = a * b

viaWhereFlipped :: Int
viaWhereFlipped = 2 <+> 3 <.> 4
  where
    infixl 7 <+>
    infixl 6 <.>
    a <+> b = a + b
    a <.> b = a * b

main :: IO ()
main = do
  print ("top level infixl", 10 >< 3 >< 2)
  print ("local infixl", let { infixl 6 <->; a <-> b = a - b } in 10 <-> 3 <-> (2 :: Int))
  print ("local infixr", let { infixr 6 <->; a <-> b = a - b } in 10 <-> 3 <-> (2 :: Int))
  print ("local precedence high", let { infixl 9 %%; a %% b = a + b } in 2 * 3 %% (4 :: Int))
  print ("local precedence low", let { infixl 1 %%; a %% b = a + b } in 2 * 3 %% (4 :: Int))
  print ("local default infixl 9", let { a %% b = a + b } in 2 * 3 %% (4 :: Int))
  print ("shadows top-level fixity", let { infixr 6 ><; a >< b = a - b } in 10 >< 3 >< (2 :: Int))
  print ("top level unchanged", 10 >< 3 >< 2)
  print ("backquoted local", let { infixr 5 `minus`; minus a b = a - b } in 10 `minus` 3 `minus` (2 :: Int))
  print ("two operators", let { infixr 5 +++, |||; a +++ b = a ++ b; a ||| b = b ++ a } in "a" +++ "b" ||| "c")
  print ("where", viaWhere, viaWhereFlipped)
  print ("fixity after definition", let { a <-> b = a - b; infixr 6 <-> } in 10 <-> 3 <-> (2 :: Int))
