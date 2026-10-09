-- Haskell 2010 Report 9 (PreludeList): lines and unlines; `unlines . lines` is not the identity
module Main (main) where

inputs :: [String]
inputs =
  [ ""
  , "\n"
  , "\n\n"
  , "a"
  , "a\n"
  , "a\nb"
  , "a\nb\n"
  , "a\n\nb"
  , "a\n\n"
  , "\na"
  , "\n\na\n\n"
  , "one two\nthree"
  , " leading\ntrailing \n"
  , "a\r\nb"
  , "tab\there\n"
  ]

main :: IO ()
main = do
  mapM_ (\s -> print ("lines", s, lines s)) inputs
  mapM_ (\s -> print ("unlines . lines", s, unlines (lines s), unlines (lines s) == s)) inputs
  print ("unlines", unlines [], unlines [""], unlines ["a"], unlines ["a", "b"])
  print ("unlines with empties", unlines ["", ""], unlines ["a", "", "b"], unlines ["", "a"])
  print ("unlines with embedded newline", unlines ["a\nb", "c"], lines (unlines ["a\nb", "c"]))
  print ("lines . unlines", all (\ls -> lines (unlines ls) == ls) [[], [""], ["a"], ["a", "", "b"], ["", ""]])
  print ("lengths", map (length . lines) inputs)
  print ("lines infinite", take 3 (lines (cycle "ab\n")), take 2 (lines (concat (repeat "x\n\n"))))
  print ("lines lazy", head (lines ("first\n" ++ undefined)))
  print ("unlines infinite", take 7 (unlines (repeat "ab")))
  print ("unlines = concatMap (++ \"\\n\")", unlines ["x", "y"] == concatMap (++ "\n") ["x", "y"])
  print ("no trailing newline is still a line", lines "last line without newline", length (lines "a\nb\nc"))
