-- Haskell 2010 Report 9 (PreludeText): the Read and Show class methods used directly (readsPrec readList showsPrec show showList)
module Main (main) where

-- a type whose lists have their own concrete syntax, for both reading and showing (as Char does for strings)
newtype Bit = Bit Bool

instance Eq Bit where
  Bit a == Bit b = a == b

instance Show Bit where
  showsPrec _ (Bit b) = showChar (if b then '1' else '0')
  showList bs = showChar '<' . foldr (\b acc -> shows b . acc) id bs . showChar '>'

instance Read Bit where
  readsPrec _ r = [(Bit (t == "1"), rest) | (t, rest) <- lex r, t == "0" || t == "1"]
  readList r = [(map (Bit . (== '1')) ds, rest) | '<' : s <- [dropWhile (== ' ') r], (ds, '>' : rest) <- [span (`elem` "01") s]]

main :: IO ()
main = do
  print ("showList Int", showList [1, 2, 3 :: Int] "", showList ([] :: [Int]) "", showList [-1 :: Int] " tail")
  print ("showList Char", showList "abc" "", showList "" "", showList "q\"" "")
  print ("showList Bool", showList [True, False] "")
  print ("showList nested", showList [[1 :: Int], []] "", showList ["a", "b"] "", showList [Just 'x'] "")
  print ("showList = show for lists", showList [1, 2 :: Int] "" == show [1, 2 :: Int], showList "s" "" == show "s")
  print ("show vs showsPrec", show (Just 1 :: Maybe Int) == showsPrec 0 (Just 1 :: Maybe Int) "", show 'x' == showsPrec 11 'x' "")
  print ("readList Int", readList "[1,2,3] tail" :: [([Int], String)], readList "[]" :: [([Int], String)])
  print ("readList Int spaces", readList " [ 1 , 2 ] " :: [([Int], String)], readList "([1])" :: [([Int], String)])
  print ("readList Int failures", readList "[1,2" :: [([Int], String)], readList "1,2" :: [([Int], String)], readList "" :: [([Int], String)])
  print ("readList Char", readList "\"abc\" tail" :: [(String, String)], readList "\"\"" :: [(String, String)])
  print ("readList Bool", readList "[True,False]" :: [([Bool], String)], readList "[True,false]" :: [([Bool], String)])
  print ("readList nested", readList "[[1],[]]" :: [([[Int]], String)], readList "[\"a\",\"b\"]" :: [([String], String)])
  print ("readList = reads for lists", (readList "[4,5] x" :: [([Int], String)]) == reads "[4,5] x")
  print ("readsPrec Bit", readsPrec 0 "1 0" :: [(Bit, String)], readsPrec 0 "2" :: [(Bit, String)])
  print ("user showList", show [Bit True, Bit False, Bit True], show ([] :: [Bit]), show (Just [Bit True]))
  print ("user readList", read "<101>" :: [Bit], read "<>" :: [Bit], read " <0> " :: [Bit])
  print ("user readList via reads", reads "<10> rest" :: [([Bit], String)], reads "[1,0]" :: [([Bit], String)], reads "<12>" :: [([Bit], String)])
  print ("user readList in structures", read "Just <11>" :: Maybe [Bit], read "(<0>,1)" :: ([Bit], Bit), read "[<1>,<>]" :: [[Bit]])
  print ("user round trip", read (show [Bit True, Bit False]) == [Bit True, Bit False], read (show [[Bit True], []]) == [[Bit True], []])
