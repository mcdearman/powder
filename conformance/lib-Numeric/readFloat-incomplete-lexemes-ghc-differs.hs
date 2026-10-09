-- Haskell 2010 Report 38.2 (Numeric): readFloat where GHC departs from the Report's reference code.
-- Report: "5." and "1e" have no parse (lexFrac/readExp fail), leading white space is not skipped, "0x10"
-- reads as 0 with rest "x10", and "NaN"/"Infinity" are accepted. GHC (which lexes one Haskell token):
-- [(5.0,".")], [(1.0,"e")], skips leading white space, reads "0x10" as 16, and rejects NaN/Infinity.
module Main (main) where

import Numeric

flt :: String -> [(Double, String)]
flt = readFloat

main :: IO ()
main = do
  print ("trailing dot", flt "5.")
  print ("dot then letter", flt "5.x")
  print ("dangling e", flt "1e")
  print ("e then letter", flt "1ex")
  print ("dangling e sign", flt "1e+")
  print ("fraction then dangling e", flt "1.5e")
  print ("leading space", flt " 2.5")
  print ("hexadecimal prefix", flt "0x10")
  print ("NaN", length (flt "NaN"))
  print ("Infinity", length (flt "Infinity"))
