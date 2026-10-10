-- Haskell 2010 Report 9 (PreludeText), 2.6: read of a number and of character literals, escapes among them
module Main (main) where

main :: IO ()
main = do
  print (read "123" :: Int)
  print (read "'a'" :: Char)
  print (read "'\\0'" :: Char)
  print (read "'\\n'" :: Char, read "'\\''" :: Char, read "'\"'" :: Char)
  print (read "'\\x41'" :: Char, read "'\\65'" :: Char, read "'\\o101'" :: Char, read "'\\SOH'" :: Char)
  print (read "\"a\\tb\\\\\"" :: String)
  print (read " -42 " :: Int, read "(7)" :: Int, read "((-7))" :: Integer)
  print (reads "'ab'" :: [(Char, String)], reads "''" :: [(Char, String)])
