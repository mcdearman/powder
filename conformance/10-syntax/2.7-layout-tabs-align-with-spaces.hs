-- Haskell 2010 Report 2.7, 10.3: a tab moves to the next column that is a multiple of eight plus one, so
-- eight spaces and two spaces with a tab indent alike
module Main (main) where

f :: ((), Char)
f = (x, y) where
        x = ()
  	y = 'y'

main :: IO ()
main = do
	print f
        print (fst f)
 	print (snd f)
