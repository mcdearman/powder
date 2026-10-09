module Config (Config(..), Mode(..), defaults, Level) where

type Level = Int

data Config = Config
  { name :: String
  , retries :: Int
  , level :: Level
  } deriving (Show, Eq)

data Mode
  = Fast { speed :: Int }
  | Slow { speed :: Int, reason :: String }
  deriving Show

defaults :: Config
defaults = Config { name = "default", retries = 3, level = 1 }
