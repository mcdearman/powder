-- Haskell 2010 Report 6.3.3 / Prelude: show of negative Double/Float inside data structures.
-- GHC DIFFERS: the Report's Prelude defines `showsPrec p = showFloat` for Float and Double, which ignores the
-- precedence, so by the letter of the Report `show (Just (-1.5))` is "Just -1.5". GHC (and Hugs) parenthesise
-- negative floating point numbers at precedence > 6, giving "Just (-1.5)".
data P = P Double Float deriving Show

main :: IO ()
main = do
  print (Just (-1.5 :: Double))
  print (Just (-1.5 :: Float))
  print (Left (-0.25) :: Either Double Int)
  print (P (-1.0) (-2.0))
  print (Just (-0.0 :: Double))
  putStrLn (showsPrec 7 (-2.5 :: Double) "")
  putStrLn (showsPrec 6 (-2.5 :: Double) "")
  print (Just (-1 / 0 :: Double))
