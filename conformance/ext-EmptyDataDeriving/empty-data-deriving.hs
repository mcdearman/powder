{-# LANGUAGE EmptyDataDeriving #-}
-- EmptyDataDeriving: instances derived for a type with no constructors
module Main (main) where

data Void deriving (Eq, Ord, Show, Read)

data Wrap a = None | Some a deriving (Eq, Ord, Show, Read)

main :: IO ()
main = do
  print (None :: Wrap Void)
  print ((None :: Wrap Void) == None, compare (None :: Wrap Void) None)
  print (reads "None" :: [(Wrap Void, String)])
  print (reads "anything" :: [(Void, String)])
  print (length ([] :: [Void]), [] == ([] :: [Void]), maximum (0 : map (const 1) ([] :: [Void])) :: Int)
