-- Haskell 2010 Report 40 (System.Exit): ExitCode(ExitSuccess, ExitFailure) and its Eq, Ord, Read and Show
-- instances (no exit is performed here)
module Main (main) where

import System.Exit

describe :: ExitCode -> String
describe ExitSuccess = "success"
describe (ExitFailure n) = "failure " ++ show n

codes :: [ExitCode]
codes = [ExitFailure 2, ExitSuccess, ExitFailure (-1), ExitFailure 1, ExitSuccess, ExitFailure 255]

main :: IO ()
main = do
  print ExitSuccess
  print (ExitFailure 3)
  print (ExitFailure (-1))
  print codes
  print (Just (ExitFailure 3))
  print (Just ExitSuccess)
  putStrLn (showsPrec 10 (ExitFailure 3) "")
  putStrLn (showsPrec 11 (ExitFailure 3) "")
  putStrLn (showsPrec 11 ExitSuccess "")
  print ("pattern matching", map describe codes)
  print ("ExitFailure is a function", map ExitFailure [1, 2, 3])
  print ("eq", ExitSuccess == ExitSuccess, ExitFailure 1 == ExitFailure 1, ExitFailure 1 == ExitFailure 2, ExitSuccess == ExitFailure 0)
  print ("neq", ExitSuccess /= ExitFailure 1, ExitFailure 1 /= ExitFailure 1)
  print ("ord constructors", ExitSuccess < ExitFailure 0, ExitSuccess < ExitFailure (-5), compare (ExitFailure 1) ExitSuccess)
  print ("ord arguments", compare (ExitFailure 1) (ExitFailure 2), compare (ExitFailure 2) (ExitFailure 2), ExitFailure 3 > ExitFailure 2)
  print ("max min", max ExitSuccess (ExitFailure 1), min ExitSuccess (ExitFailure 1), maximum codes, minimum codes)
  print ("read", (read "ExitSuccess" :: ExitCode), (read "ExitFailure 3" :: ExitCode))
  print ("read with spaces and parens", (read " ( ExitFailure 42 ) " :: ExitCode), (read "(ExitSuccess)" :: ExitCode))
  print ("read negative", (read "ExitFailure (-1)" :: ExitCode))
  print ("read list", (read "[ExitSuccess,ExitFailure 1]" :: [ExitCode]))
  print ("read . show", all (\c -> read (show c) == c) codes)
  print ("reads rest", (reads "ExitFailure 7 rest" :: [(ExitCode, String)]))
  print ("reads failure", (reads "ExitFailure" :: [(ExitCode, String)]))
  print ("readsPrec 11 needs parentheses", null (readsPrec 11 "ExitFailure 7" :: [(ExitCode, String)]))
