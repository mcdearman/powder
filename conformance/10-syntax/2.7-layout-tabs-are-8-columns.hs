-- Haskell 2010 Report 10.3 note 1: for layout, tab stops are 8 columns apart; a tab aligns with 8 spaces
-- (this file intentionally contains TAB characters in indentation)
module Main (main) where

f :: Int -> Int
f x = a + b
  where
	a = x
        b = 2
	c = 3

g :: Int -> Int
g x = case x of
	0 -> 10
        1 -> 11
	_ -> 12

-- a tab after 4 spaces also advances to column 9
h :: Int
h = let
    	p = 1
        q = 2
     in p + q

main :: IO ()
main = do
	print ("f", f 1)
        print ("g", g 0, g 1, g 2)
	print ("h", h)
