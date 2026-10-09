-- Haskell 2010 Report 4.2.1: data declarations with no constructors (new in Haskell 2010), used as phantom types
module Main (main) where

data Empty

data Phantom a

data Metres

data Feet

newtype Len u = Len Int

data Tag t = Tag

addLen :: Len u -> Len u -> Len u
addLen (Len a) (Len b) = Len (a + b)

metres :: Int -> Len Metres
metres = Len

feet :: Int -> Len Feet
feet = Len

showMetres :: Len Metres -> String
showMetres (Len n) = show n ++ "m"

showFeet :: Len Feet -> String
showFeet (Len n) = show n ++ "ft"

describe :: Maybe Empty -> String
describe Nothing = "nothing"
describe (Just _) = "impossible"

countEmpties :: [Empty] -> Int
countEmpties = length

fromLeft :: Either Int Empty -> Int
fromLeft (Left n) = n
fromLeft (Right _) = 0

tagName :: Tag (Phantom Empty) -> String
tagName Tag = "tag"

main :: IO ()
main = do
  print ("metres", showMetres (addLen (metres 2) (metres 40)))
  print ("feet", showFeet (addLen (feet 1) (feet 2)))
  print ("maybe empty", describe Nothing)
  print ("list of empty", countEmpties [])
  print ("either", fromLeft (Left 7))
  print ("phantom tag", tagName Tag)
