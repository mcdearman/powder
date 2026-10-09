-- Haskell 2010 Report 4.5.1: declaration groups are split by dependency analysis before generalisation
module Main (main) where

-- `user` is declared first and uses `poly` at two types; this only typechecks if
-- `poly` is generalised in its own group before `user` is checked.
user :: (Char, Bool)
user = (poly 'a', poly True)

poly x = x

-- three levels of dependency, declared in reverse order
top :: ((Int, Int), (Char, Char))
top = (middle 1, middle 'x')

middle y = bottom y y

bottom p q = (p, q)

-- genuinely mutually recursive: one group
isEven, isOdd :: Int -> Bool
isEven 0 = True
isEven n = isOdd (n - 1)
isOdd 0 = False
isOdd n = isEven (n - 1)

-- `shown` has a signature, so `caller` does not depend on it for grouping purposes:
-- although they call each other, `caller` may use `shown` at two different types.
shown :: Show a => a -> Int -> String
shown x n = if n <= 0 then show x else caller n

caller n = shown n (n - 1) ++ shown (n > 0) 0

main :: IO ()
main = do
  print ("top-level", user)
  print ("three levels", top)
  print ("mutual recursion", isEven 10, isOdd 10, isEven 7)
  print ("signature breaks group", shown 'x' 1, caller 2)
  let pairUp = (ident 'a', ident True)
      ident x = x
  print ("let group", pairUp)
  let a = (b (1 :: Int), b 'x')
      b y = c y y
      c p q = (p, q)
  print ("let three levels", a)
  print ("where group", whereGroup)

whereGroup :: (Char, String)
whereGroup = inWhere
  where
    inWhere = (konst 'k' True, konst "s" 'c')
    konst v _ = v
