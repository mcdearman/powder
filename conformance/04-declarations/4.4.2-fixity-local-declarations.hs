-- Haskell 2010 Report 4.4.2: fixity declarations in let and where; a local rebinding does not inherit the outer fixity
module Main (main) where

infixr 5 |+|

(|+|) :: String -> String -> String
a |+| b = "(" ++ a ++ b ++ ")"

outer :: String
outer = "a" |+| "b" |+| "c"

-- rebinding without a fixity declaration: the local operator is infixl 9
shadowedDefault :: String
shadowedDefault =
  let a |+| b = "[" ++ a ++ b ++ "]"
  in "a" |+| "b" |+| "c"

-- rebinding with its own fixity declaration
shadowedLeft :: String
shadowedLeft =
  let infixl 5 |+|
      a |+| b = "<" ++ a ++ b ++ ">"
  in "a" |+| "b" |+| "c"

localLet :: Int
localLet =
  let infixr 5 +++
      x +++ y = x - y
  in 10 +++ 3 +++ 2

localWhere :: Int
localWhere = 10 <-> 3 <-> 2 + 1
  where
    infixl 7 <->
    a <-> b = a - b

localSameLevelAsTimes :: Int
localSameLevelAsTimes = 2 * 10 <-> 3
  where
    a <-> b = a - b
    infixl 7 <->

localBackquoted :: Bool
localBackquoted =
  let infix 4 `eq`
      eq :: Int -> Int -> Bool
      eq a b = a == b
  in 1 + 1 `eq` 2

localSeveral :: Int
localSeveral = 2 `mul` 3 `add` 4 `mul` 5
  where
    infixl 6 `add`
    infixl 7 `mul`
    add, mul :: Int -> Int -> Int
    add = (+)
    mul = (*)

main :: IO ()
main = do
  print ("top-level infixr 5", outer)
  print ("local rebinding defaults to infixl 9", shadowedDefault)
  print ("local rebinding with infixl 5", shadowedLeft)
  print ("outer unaffected", "x" |+| "y" |+| "z")
  print ("let infixr", localLet)
  print ("where infixl 7", localWhere)
  print ("where same level as *", localSameLevelAsTimes)
  print ("let backquoted infix 4", localBackquoted)
  print ("where several", localSeveral)
