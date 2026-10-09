-- Haskell 2010 Report 3.4, 10.6: prefix negation has precedence 6, so it cannot be the right operand of an
-- operator of precedence 9, which a function in backquotes is by default
main :: IO ()
main = print (subtract `id` -4 $ 1)
