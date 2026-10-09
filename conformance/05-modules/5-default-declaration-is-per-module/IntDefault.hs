module IntDefault (inIntModule, polyShow) where

default (Int)

-- 2 ^ 64 wraps to 0 at Int, and maxBound-style overflow shows the type chosen
inIntModule :: (String, String)
inIntModule = (show (2 ^ 64), show (9223372036854775807 + 1))

polyShow :: (Show a, Num a) => a -> String
polyShow x = show (x + 1)
