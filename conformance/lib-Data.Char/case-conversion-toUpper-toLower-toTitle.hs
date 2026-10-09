-- Haskell 2010 Report 16 (Data.Char): toUpper, toLower, toTitle
module Main (main) where

import Data.Char

printable :: String
printable = [' ' .. '~']

main :: IO ()
main = do
  print ("toUpper ascii", map toUpper printable)
  print ("toLower ascii", map toLower printable)
  print ("toTitle ascii", map toTitle printable)
  print ("toUpper controls unchanged", all (\c -> toUpper c == c) ['\0' .. '\31'])
  print ("toLower controls unchanged", all (\c -> toLower c == c) ['\0' .. '\31'])
  print ("toUpper idempotent", all (\c -> toUpper (toUpper c) == toUpper c) ['\0' .. '\127'])
  print ("toLower idempotent", all (\c -> toLower (toLower c) == toLower c) ['\0' .. '\127'])
  print ("round trip on letters", all (\c -> toLower (toUpper c) == c) ['a' .. 'z'], all (\c -> toUpper (toLower c) == c) ['A' .. 'Z'])
  print ("toTitle equals toUpper on ascii", all (\c -> toTitle c == toUpper c) ['\0' .. '\127'])
  print ("word", map toUpper "Hello, World! 123", map toLower "Hello, World! 123")
  print ("latin1 a grave", ord (toUpper '\224'), ord (toLower '\192'))
  print ("latin1 thorn", ord (toUpper '\254'), ord (toLower '\222'))
  print ("multiplication and division signs unchanged", ord (toUpper '\247'), ord (toLower '\215'))
  print ("sharp s has no single upper case", ord (toUpper '\223'))
  print ("y diaeresis leaves latin1", ord (toUpper '\255'), ord (toLower '\376'))
  print ("micro sign maps to greek capital mu", ord (toUpper '\181'))
  print ("greek", ord (toUpper '\945'), ord (toLower '\913'), ord (toTitle '\945'))
  print ("cyrillic", ord (toUpper '\1072'), ord (toLower '\1040'))
  print ("dz digraph upper", ord (toUpper '\454'), ord (toUpper '\453'), ord (toUpper '\452'))
  print ("dz digraph lower", ord (toLower '\452'), ord (toLower '\453'), ord (toLower '\454'))
  print ("dz digraph title", ord (toTitle '\452'), ord (toTitle '\453'), ord (toTitle '\454'))
  print ("digits unchanged", map toUpper "0123456789" == "0123456789", map toTitle "42" == "42")
