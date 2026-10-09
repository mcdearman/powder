-- Haskell 2010 Report 9 (PreludeList): words and unwords; words splits on isSpace characters and drops empty words
module Main (main) where

inputs :: [String]
inputs =
  [ ""
  , " "
  , "   "
  , "a"
  , " a"
  , "a "
  , "  a  "
  , "a b"
  , "a  b"
  , "a\tb"
  , "a\nb"
  , "a \t\n b"
  , "  hello   world  "
  , "one,two three"
  , "a\r\nb\fc\vd"
  , "\n\n"
  , "x y z"
  ]

main :: IO ()
main = do
  mapM_ (\s -> print ("words", s, words s)) inputs
  mapM_ (\s -> print ("unwords . words", s, unwords (words s), unwords (words s) == s)) inputs
  print ("unwords", unwords [], unwords [""], unwords ["a"], unwords ["a", "b"])
  print ("unwords with empties", unwords ["", ""], unwords ["a", "", "b"], unwords ["", "a"], unwords ["a", ""])
  print ("unwords keeps inner spaces", unwords ["a b", "c"], words (unwords ["a b", "c"]))
  print ("words . unwords", all (\ws -> words (unwords ws) == ws) [[], ["a"], ["a", "b", "c"], ["hello", "world"]])
  print ("words . unwords with empty word", words (unwords ["a", "", "b"]))
  print ("counts", map (length . words) inputs)
  print ("words infinite", take 3 (words (cycle "ab ")), take 2 (words (concat (repeat "  x"))))
  print ("words lazy", head (words ("first " ++ undefined)))
  print ("unwords infinite", take 8 (unwords (repeat "ab")))
  print ("words of lines", map words (lines "a b\n\n c  d \n"))
  print ("non-space punctuation kept", words "it's a (small) test-case; ok.")
  print ("normalise", unwords (words "  many    spaces\there  "))
