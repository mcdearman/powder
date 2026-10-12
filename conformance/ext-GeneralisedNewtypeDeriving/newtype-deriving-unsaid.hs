{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
-- GeneralisedNewtypeDeriving: with nothing said of how, a class the Report cannot derive is derived for a newtype as
-- the instance of what it is a newtype of
module Main (main) where

newtype Count = Count Integer deriving (Show, Eq, Ord, Num, Enum, Real, Integral)

newtype Text' = Text' String deriving (Eq, Ord, Semigroup, Monoid)

newtype Parser a = Parser (Maybe a) deriving (Show, Functor, Applicative, Monad)

unText :: Text' -> String
unText (Text' s) = s

main :: IO ()
main = do
  print (Count 2 + 3, Count 5 * 5 - 1, sum [Count 1, Count 2, Count 3])
  print ([Count 1 .. Count 4], Count 7 `divMod` Count 2, toInteger (Count 9))
  -- Show is the Report's to derive, and says the constructor.
  print (Count 0)
  putStrLn (unText (Text' "ab" <> Text' "cd" <> mempty))
  print (Text' "a" < Text' "b", mconcat [Text' "x", Text' "y"] == Text' "xy")
  print (Parser (Just 1) >>= \x -> Parser (Just (x + 1 :: Int)))
  print ((,) <$> Parser (Just 'a') <*> Parser (Nothing :: Maybe Int))
