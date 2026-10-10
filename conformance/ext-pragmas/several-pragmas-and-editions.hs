{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE LambdaCase, TupleSections #-}
{-# language MultiWayIf #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
-- LANGUAGE pragmas: several in one, an edition, the word in lower case, and a pragma that is not one of them
module Main (main) where

main :: IO ()
main = do
  print (map (\case 0 -> 'z'; _ -> 'n') [0, 1 :: Int])
  print (map (,'x') [True])
  print (if | otherwise -> "guards")
