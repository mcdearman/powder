-- Haskell 2010 Report 4.2.3: matching a newtype's constructor looks at nothing; a newtype with a field
-- label has a selector
module Main (main) where

newtype Box = Box ()

newtype Name = Name {runName :: String}

data Strict = Strict ()

ignore, repack, select :: String
ignore = case undefined of
  Box _ -> "new"
repack = case Name "ty" of
  Name s -> s
select = runName (Name "pe")

main :: IO ()
main = do
  putStrLn (ignore ++ repack ++ select)
  print (case Box undefined of Box _ -> "the field is not looked at either")
  print (case Strict undefined of Strict _ -> "nor is a data constructor's field, unmatched")
  print (runName (Name "a") ++ runName ((Name "b") {runName = "c"}))
  print ((\(Box _) -> 'k') undefined)
