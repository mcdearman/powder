-- Haskell 2010 Report 13: guard :: MonadPlus m => Bool -> m (); the Report gives MonadPlus instances
-- only for lists and Maybe, so guard at Either String has no instance
module Main (main) where

import Control.Monad

check :: Int -> Either String ()
check n = guard (n > 0)

main :: IO ()
main = print (check 1)
