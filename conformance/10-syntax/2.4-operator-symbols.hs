-- Haskell 2010 Report 2.4: operator symbols built from any of ! # $ % & * + . / < = > ? @ \ ^ | - ~ :
module Main (main) where

infixl 6 !, #, %, &, +++, ?, @@, \\\, ^^^, |||, -:, ~~, <=>, .:., $$, */, ==>, <-->, !!!, ...

(!), (#), (%), (&), (+++), (?), (@@), (\\\), (^^^), (|||), (-:), (~~) :: Int -> Int -> Int
a ! b = a + b
a # b = a - b
a % b = a * b
a & b = a + 2 * b
a +++ b = a + 3 * b
a ? b = a + 4 * b
a @@ b = a + 5 * b
a \\\ b = a + 6 * b
a ^^^ b = a + 7 * b
a ||| b = a + 8 * b
a -: b = a + 9 * b
a ~~ b = a + 10 * b

(<=>), (.:.), ($$), (*/), (==>), (<-->), (!!!), (...) :: Int -> Int -> Int
a <=> b = a + 11 * b
a .:. b = a + 12 * b
a $$ b = a + 13 * b
a */ b = a + 14 * b
a ==> b = a + 15 * b
a <--> b = a + 16 * b
a !!! b = a + 17 * b
a ... b = a + 19 * b

-- operators that merely contain reserved operators as substrings are ordinary operators
(=>>), (<--), (-->), (|>), (<|), (\\), (~>), (@>), (=.=), (.=), (-<), (<-<) :: Int -> Int -> Int
a =>> b = a + 20 * b
a <-- b = a + 21 * b
a --> b = a + 22 * b
a |> b = a + 24 * b
a <| b = a + 25 * b
a \\ b = a + 26 * b
a ~> b = a + 27 * b
a @> b = a + 28 * b
a =.= b = a + 29 * b
a .= b = a + 30 * b
a -< b = a + 31 * b
a <-< b = a + 32 * b

-- constructor operators start with a colon
data P = Int :| Int | Int :-> Int | Int :::: Int | Int :. Int | Int :=: Int deriving Show

main :: IO ()
main = do
  print ("single", 1 ! 1, 1 # 1, 1 % 1, 1 & 1, 1 +++ 1, 1 ? 1)
  print ("more", 1 @@ 1, 1 \\\ 1, 1 ^^^ 1, 1 ||| 1, 1 -: 1, 1 ~~ 1)
  print ("multi", 1 <=> 1, 1 .:. 1, 1 $$ 1, 1 */ 1, 1 ==> 1, 1 <--> 1)
  print ("multi2", 1 !!! 1, 1 ... 1)
  print ("reserved-substring", 1 =>> 1, 1 <-- 1, 1 --> 1, 1 |> 1, 1 <| 1, 1 \\ 1)
  print ("reserved-substring2", 1 ~> 1, 1 @> 1, 1 =.= 1, 1 .= 1, 1 -< 1, 1 <-< 1)
  print ("prefix use", (!) 1 2, (+++) 1 2, (<-->) 1 2)
  print ("sections", (! 1) 2, (1 !) 2, (\\\ 1) 2)
  print ("consyms", 1 :| 2, 1 :-> 2, 1 :::: 2)
  print ("consyms2", 1 :. 2, 1 :=: 2, (:|) 3 4)
