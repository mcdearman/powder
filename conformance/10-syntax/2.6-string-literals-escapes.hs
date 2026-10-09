-- Haskell 2010 Report 2.6: string literals: escapes, \& (empty), maximal-munch of numeric and \SO/\SOH escapes
module Main (main) where

import Data.Char (ord)

main :: IO ()
main = do
  print ("empty", "", length "")
  print ("plain", "hello, world")
  print ("charesc", "\a\b\f\n\r\t\v\\\"\'")
  print ("single quote unescaped", "it's", "it\'s" == "it's")
  print ("numeric", "\65\66\67", "\x41\x42", "\o101\o102")
  print ("ampersand is empty", "\&", length "\&", length "a\&b", "a\&b" == "ab")
  print ("numeric then digit", map ord "\1234", map ord "\12\&34", map ord "\x41\&1")
  print ("SO then H", map ord "\SO\&H", map ord "\SOH", map ord "\SO")
  print ("SO followed by other", map ord "\SOx", map ord "\SO H")
  print ("control", map ord "\^A\^Z\^@\^[\^\\^]\^^\^_")
  print ("ascii names", map ord "\NUL\SOH\STX\ETX\DEL\ESC\SP")
  print ("show roundtrip 1", show "\SO\&H", show "\SOH", show "\1234\&5")
  print ("show roundtrip 2", show "a\nb\tc", show "q\"q", show "back\\slash")
  print ("show high", show "\200\&1", show "\1114111")
  print ("comment chars in string", "-- not a comment", "{- nor this -}")
  print ("string is list of char", "abc" == ['a', 'b', 'c'], "abc" == 'a' : "bc")
  print ("hex digit maximal munch", map ord "\x41g", map ord "\x4a\&b")
  print ("octal maximal munch", map ord "\o1018", map ord "\o101\&7")
  print ("backslash at end", last "abc\\", length "\\\\")
