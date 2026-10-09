-- Haskell 2010 Report 3.5 / 10.6: sections: (op e), (e op), backquoted sections, and (- e) being negation not a section
module Main (main) where

infixr 5 +++
infixl 6 `minus`

(+++) :: [a] -> [a] -> [a]
(+++) = (++)

minus :: Int -> Int -> Int
minus = (-)

main :: IO ()
main = do
  print ("right sections", (+ 1) 2, (* 2) 5, (^ 2) 5, (/ 2) 5)
  print ("left sections", (1 +) 2, (2 *) 5, (2 ^) 5, (2 /) 5)
  print ("subtract vs negative literal", (subtract 1) 5, (- 1), (\x -> x - 1) 5)
  print ("left minus section", (10 -) 3, (10 - 1 -) 3)
  print ("backquote right", (`div` 2) 9, (`mod` 3) 10, (`elem` "abc") 'b')
  print ("backquote left", (9 `div`) 2, (10 `mod`) 3, ('b' `elem`) "abc")
  print ("user backquote", (`minus` 1) 10, (10 `minus`) 1, (10 `minus` 2 `minus`) 3)
  print ("comparison sections", filter (> 2) [1, 2, 3, 4], filter (2 >) [1, 2, 3, 4])
  print ("cons sections", (1 :) [2, 3], (: [2, 3]) 1, (: []) 'x')
  print ("append sections", ("ab" ++) "cd", (++ "cd") "ab")
  print ("user infixr section", ([1] +++) [2, 3],(+++ [2] +++ [3]) [1])
  print ("compose sections", ((+ 1) .) (* 2) 5, (. (+ 1)) (* 2) 5)
  print ("dollar sections", ($ 3) (+ 1), (negate $) 3, map ($ 2) [(+ 1), (* 3)])
  print ("section operand application", (subtract 2 . negate) 5, (+ length "abc") 1)
  print ("section with tighter operand", (+ 2 * 3) 1, (2 * 3 +) 1, (: 1 + 1 : []) 0)
  print ("section with lambda operand", (+ (\x -> x) 1) 1)
  print ("and/or sections", (True &&) False, (|| True) False)
  print ("index section", (!! 1) "abc", ("abc" !!) 2)
  print ("tuple constructor", (,) 1 'a', (,,) 1 'a' "s")
  print ("operator as value", foldr (+) 0 [1, 2, 3], zipWith (-) [5, 6] [1, 2])
  print ("section equivalences", map (subtract 1) [1, 2] == map (\x -> x - 1) [1, 2])
  print ("eta", map (2 ^) [1, 2, 3] == map (\y -> 2 ^ y) [1, 2, 3])
  print ("section with negative operand", (+ (-1)) 5, (subtract (-1)) 5, ((-1) +) 5)
  print ("minus-prefixed left section", (- 1 +) 5, (- 2 * 3 +) 1)
