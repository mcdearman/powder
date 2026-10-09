-- Haskell 2010 Report 4.4.2: infixl, infixr and infix declarations at every precedence level 0 to 9
module Main (main) where

infixl 0 `l0`
infixl 1 `l1`
infixl 2 `l2`
infixl 3 `l3`
infixl 4 `l4`
infixl 5 `l5`
infixl 6 `l6`
infixl 7 `l7`
infixl 8 `l8`
infixl 9 `l9`

infixr 0 `r0`
infixr 1 `r1`
infixr 2 `r2`
infixr 3 `r3`
infixr 4 `r4`
infixr 5 `r5`
infixr 6 `r6`
infixr 7 `r7`
infixr 8 `r8`
infixr 9 `r9`

infix 0 `n0`
infix 1 `n1`
infix 2 `n2`
infix 3 `n3`
infix 4 `n4`
infix 5 `n5`
infix 6 `n6`
infix 7 `n7`
infix 8 `n8`
infix 9 `n9`

op :: String -> String -> String -> String
op name a b = "(" ++ a ++ " " ++ name ++ " " ++ b ++ ")"

l0, l1, l2, l3, l4, l5, l6, l7, l8, l9 :: String -> String -> String
l0 = op "l0"
l1 = op "l1"
l2 = op "l2"
l3 = op "l3"
l4 = op "l4"
l5 = op "l5"
l6 = op "l6"
l7 = op "l7"
l8 = op "l8"
l9 = op "l9"

r0, r1, r2, r3, r4, r5, r6, r7, r8, r9 :: String -> String -> String
r0 = op "r0"
r1 = op "r1"
r2 = op "r2"
r3 = op "r3"
r4 = op "r4"
r5 = op "r5"
r6 = op "r6"
r7 = op "r7"
r8 = op "r8"
r9 = op "r9"

n0, n1, n2, n3, n4, n5, n6, n7, n8, n9 :: String -> String -> String
n0 = op "n0"
n1 = op "n1"
n2 = op "n2"
n3 = op "n3"
n4 = op "n4"
n5 = op "n5"
n6 = op "n6"
n7 = op "n7"
n8 = op "n8"
n9 = op "n9"

main :: IO ()
main = do
  putStrLn ("l ascending: " ++ ("a" `l0` "b" `l1` "c" `l2` "d" `l3` "e" `l4` "f" `l5` "g" `l6` "h" `l7` "i" `l8` "j" `l9` "k"))
  putStrLn ("l descending: " ++ ("a" `l9` "b" `l8` "c" `l7` "d" `l6` "e" `l5` "f" `l4` "g" `l3` "h" `l2` "i" `l1` "j" `l0` "k"))
  putStrLn ("r ascending: " ++ ("a" `r0` "b" `r1` "c" `r2` "d" `r3` "e" `r4` "f" `r5` "g" `r6` "h" `r7` "i" `r8` "j" `r9` "k"))
  putStrLn ("r descending: " ++ ("a" `r9` "b" `r8` "c" `r7` "d" `r6` "e" `r5` "f" `r4` "g" `r3` "h" `r2` "i" `r1` "j" `r0` "k"))
  putStrLn ("n ascending: " ++ ("a" `n0` "b" `n1` "c" `n2` "d" `n3` "e" `n4` "f" `n5` "g" `n6` "h" `n7` "i" `n8` "j" `n9` "k"))
  putStrLn ("n descending: " ++ ("a" `n9` "b" `n8` "c" `n7` "d" `n6` "e" `n5` "f" `n4` "g" `n3` "h" `n2` "i" `n1` "j" `n0` "k"))
  print ("l assoc 0-4", ["a" `l0` "b" `l0` "c", "a" `l1` "b" `l1` "c", "a" `l2` "b" `l2` "c", "a" `l3` "b" `l3` "c", "a" `l4` "b" `l4` "c"])
  print ("l assoc 5-9", ["a" `l5` "b" `l5` "c", "a" `l6` "b" `l6` "c", "a" `l7` "b" `l7` "c", "a" `l8` "b" `l8` "c", "a" `l9` "b" `l9` "c"])
  print ("r assoc 0-4", ["a" `r0` "b" `r0` "c", "a" `r1` "b" `r1` "c", "a" `r2` "b" `r2` "c", "a" `r3` "b" `r3` "c", "a" `r4` "b" `r4` "c"])
  print ("r assoc 5-9", ["a" `r5` "b" `r5` "c", "a" `r6` "b" `r6` "c", "a" `r7` "b" `r7` "c", "a" `r8` "b" `r8` "c", "a" `r9` "b" `r9` "c"])
  print ("mixed l r n", "a" `r3` "b" `l4` "c" `r3` "d" `n5` "e")
  print ("n between l", "a" `l6` "b" `n5` "c" `l6` "d")
  print ("n between r", "a" `r2` "b" `n1` "c" `r2` "d")
  print ("zigzag", "a" `l9` "b" `r0` "c" `l9` "d" `r0` "e" `n4` "f")
  print ("level 9 vs application", id "a" `l9` id "b")
  print ("level 0 vs $-like nesting", "a" `r0` "b" `l1` "c" `r0` "d")
