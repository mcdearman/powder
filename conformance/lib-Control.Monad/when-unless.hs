-- Haskell 2010 Report 13 (Control.Monad): when, unless
module Main (main) where

import Control.Monad

report :: Int -> IO ()
report n = do
  when (n > 0) (putStrLn (show n ++ " is positive"))
  unless (n > 0) (putStrLn (show n ++ " is not positive"))
  when (even n) $ do
    putStr (show n)
    putStrLn " is even"

main :: IO ()
main = do
  mapM_ report [-1, 0, 1, 2]
  when False (putStrLn "never printed")
  unless True (putStrLn "never printed either")
  u <- when False (putStrLn "x")
  print ("result of when False", u)
  print ("when true nothing", when True (Nothing :: Maybe ()))
  print ("when false nothing", when False (Nothing :: Maybe ()))
  print ("when true just", when True (Just ()))
  print ("unless true nothing", unless True (Nothing :: Maybe ()))
  print ("unless false nothing", unless False (Nothing :: Maybe ()))
  print ("when true list", when True [(), ()])
  print ("when false list", when False [(), ()])
  print ("unless false list", unless False ([] :: [()]))
  print ("when False ignores its action", when False (undefined :: Maybe ()))
