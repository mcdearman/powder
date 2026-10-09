Haskell 2010 Report 10.4: literate source, "bird track" style. Only lines
beginning with ">" are program text; everything else is commentary.

> module Main (main) where

This paragraph is a comment. It may contain anything: let x = in where {{{ "unterminated

> import Data.Char (toUpper)
>
> shout :: String -> String
> shout = map toUpper

The layout rule applies to the program text with the ">" replaced by a space.

> classify :: Int -> String
> classify n
>   | n < 0 = "negative"
>   | n == 0 = "zero"
>   | otherwise = "positive"
>   where
>     _unused = ()

A code line may contain comments of its own:

> -- an ordinary line comment inside the code
> {- and a block
>    comment -}
> main :: IO ()
> main = do
>   print ("shout", shout "literate")
>   print ("classify", map classify [-1, 0, 1])
>   print ("operator section with >", filter (> 1) [1, 2, 3 :: Int])

The end.
