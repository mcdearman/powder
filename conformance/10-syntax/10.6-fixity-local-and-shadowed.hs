-- Haskell 2010 Report 4.4.2 / 10.6: fixity declarations in let/where/class scope the binding they accompany; shadowing resets fixity
module Main (main) where

infixl 6 <+>

(<+>) :: Int -> Int -> Int
a <+> b = a * 10 + b

class Pretty a where
  infixr 5 <->
  (<->) :: a -> a -> a
  infixl 6 `beside`
  beside :: a -> a -> a

instance Pretty [b] where
  xs <-> ys = xs ++ ys
  beside xs ys = ys ++ xs

localLet :: String
localLet =
  let infixr 5 <+>
      x <+> y = "(" ++ x ++ "+" ++ y ++ ")"
   in "a" <+> "b" <+> "c"

localWhere :: String
localWhere = "a" <+> "b" <+> "c"
  where
    infixl 5 <+>
    x <+> y = "(" ++ x ++ "+" ++ y ++ ")"

-- a local rebinding without a fixity declaration gets the default infixl 9, not the outer fixity
localDefault :: Int
localDefault =
  let a <+> b = a - b
   in 2 * 10 <+> 3 <+> 1

localBackquote :: Int
localBackquote = 100 `sub` 10 `sub` 1
  where
    infixr 6 `sub`
    sub a b = a - b

main :: IO ()
main = do
  print ("top-level infixl 6", 1 <+> 2 <+> 3)
  print ("let infixr", localLet)
  print ("where infixl", localWhere)
  print ("local default infixl 9", localDefault)
  print ("local backquote infixr", localBackquote)
  print ("class method infixr 5", [1] <-> [2] <-> [3 :: Int])
  print ("class method backquote infixl 6", "a" `beside` "b" `beside` "c")
  print ("class fixities mix", "a" `beside` "b" <-> "c" `beside` "d")
  let infix 4 ===
      x === y = x == (y :: Int)
  print ("do-let fixity", 1 + 1 === 2, 2 === 1 + 1)
