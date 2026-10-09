-- Haskell 2010 Report 3.7: [e1, ..., ek] = e1 : (... (ek : [])); (:) and [] are the list constructors
module Main (main) where

prefixTyped :: [] Int
prefixTyped = [4, 5]

main :: IO ()
main = do
  print ("literal", [1, 2, 3])
  print ("cons", 1 : 2 : 3 : [])
  print ("equal", [1, 2, 3] == 1 : 2 : 3 : [])
  print ("mixed", 1 : [2, 3], 1 : 2 : [3])
  print ("empty", [] :: [Int])
  print ("null", null [], null [[]], null [()])
  print ("singleton", [7], 7 : [], [7] == 7 : [])
  print ("nested", [[1, 2], [], [3]])
  print ("nested lengths", map length [[1, 2], [], [3]])
  print ("triple nested", [[[1]], [[2, 3], []]])
  print ("nested cons", [1] : [2, 3] : [])
  print ("string", ['a', 'b', 'c'], "abc" == 'a' : 'b' : 'c' : [])
  print ("empty string", "" == ([] :: String))
  print ("list of tuples", [(1, 'a'), (2, 'b')])
  print ("list of units", [(), ()])
  print ("list of maybes", [Just 1, Nothing])
  print ("list of functions", map (\f -> f 10) [(+ 1), (* 2), subtract 3])
  print ("cons function", (:) 'x' "yz", foldr (:) [] [1, 2, 3])
  print ("prefix type", prefixTyped)
  print ("element exprs", [1 + 1, 2 * 3, if True then 4 else 5, let n = 6 in n])
  print ("head/tail", head [1, 2, 3], tail [1, 2, 3])
  print ("cons pattern", case [1, 2, 3] of { (a : b : rest) -> (a, b, rest); _ -> (0, 0, []) })
  print ("list pattern", case "hi" of { [c1, c2] -> [c2, c1]; _ -> "" })
  print ("nil pattern", case "" of { [] -> "nil"; _ -> "cons" })
  print ("ordering", [1, 2] < [1, 3], [1] < [1, 0], [] < [0], compare "b" "ab")
  print ("lazy elements", length [undefined, undefined :: Int])
  print ("long", sum [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20])
