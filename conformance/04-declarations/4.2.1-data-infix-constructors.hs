-- Haskell 2010 Report 4.2.1: infix constructor operators (:+:) and backquoted constructors in data declarations
module Main (main) where

infixr 5 :+:
infixl 6 :*:

data Expr = Lit Int
          | Expr :+: Expr
          | Expr :*: Expr
          | Expr `Minus` Expr

data Ne a = a :| [a]

data Assoc k v = k :-> v

render :: Expr -> String
render (Lit n) = show n
render (a :+: b) = "(" ++ render a ++ "+" ++ render b ++ ")"
render ((:*:) a b) = "(" ++ render a ++ "*" ++ render b ++ ")"
render (a `Minus` b) = "(" ++ render a ++ "-" ++ render b ++ ")"

eval :: Expr -> Int
eval (Lit n) = n
eval (a :+: b) = eval a + eval b
eval (a :*: b) = eval a * eval b
eval (Minus a b) = eval a - eval b

neHead :: Ne a -> a
neHead (x :| _) = x

neToList :: Ne a -> [a]
neToList (x :| xs) = x : xs

key :: Assoc k v -> k
key (k :-> _) = k

value :: Assoc k v -> v
value ((:->) _ v) = v

main :: IO ()
main = do
  print ("infix use", render (Lit 1 :+: Lit 2))
  print ("prefix use", render ((:+:) (Lit 1) (Lit 2)))
  print ("precedence", render (Lit 1 :+: Lit 2 :*: Lit 3))
  print ("precedence 2", render (Lit 1 :*: Lit 2 :+: Lit 3))
  print ("right assoc", render (Lit 1 :+: Lit 2 :+: Lit 3))
  print ("left assoc", render (Lit 1 :*: Lit 2 :*: Lit 3))
  print ("backquoted", render (Lit 10 `Minus` Lit 3 `Minus` Lit 2))
  print ("backquoted prefix", render (Minus (Lit 10) (Lit 3)))
  print ("mixed", render (Lit 1 :+: Lit 2 `Minus` Lit 3))
  print ("eval", eval (Lit 1 :+: Lit 2 :*: Lit 3), eval (Lit 10 `Minus` Lit 3 `Minus` Lit 2))
  print ("left section", render ((Lit 1 :+:) (Lit 2)))
  print ("right section", render ((:*: Lit 2) (Lit 1)))
  print ("as function", map render (zipWith (:+:) [Lit 1, Lit 2] [Lit 3, Lit 4]))
  print ("ne head", neHead ('a' :| "bc"))
  print ("ne list", neToList ((1 :: Int) :| [2, 3]))
  print ("assoc", key ("k" :-> True), value ("k" :-> True))
  print ("assoc map", map key (zipWith (:->) "ab" [1 :: Int, 2]))
