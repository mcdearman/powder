-- Haskell 2010 Report 6.3.3: showList; the default [a,b,c] form, Char's string form, and a user override
data Bit = O | I

instance Show Bit where
  showsPrec _ O = showChar '0'
  showsPrec _ I = showChar '1'
  showList bs = showString "0b" . foldr (.) id (map shows bs)

-- default showList
data Plain = Plain

instance Show Plain where
  showsPrec _ Plain = showString "plain"

main :: IO ()
main = do
  print [O, I, I]
  print ([] :: [Bit])
  print O
  print (Just [I, O])
  print [[O], [I, I]]
  print (O, [I])
  print [Just I]
  putStrLn (showList [I, O, I] "")
  putStrLn (showList [I] " tail")
  putStrLn (showsPrec 11 [O, O] "")
  print [Plain, Plain]
  print ([] :: [Plain])
  putStrLn (showList [Plain] "")
  putStrLn (showList "chars" "")
  putStrLn (showList [1, 2, 3 :: Int] "")
  putStrLn (showList ([] :: [Int]) "")
  putStrLn (showList "" "")
  putStrLn (showList [True, False] "!")
  putStrLn (showList ["a", "b"] "")
  putStrLn (showList [Just 'x'] "")
  print (show "abc" == showList "abc" "", show [LT, GT] == showList [LT, GT] "")
  print (['a', 'b'], [['c']], [("d", 'e')])
