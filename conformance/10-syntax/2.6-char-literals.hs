-- Haskell 2010 Report 2.6: character literals and all escape forms
module Main (main) where

import Data.Char (ord)

main :: IO ()
main = do
  print ("plain", 'a', 'Z', '0', ' ', '~')
  print ("symbols", '"', '-', '{', '}', '`')
  print ("charesc", '\a', '\b', '\f', '\n', '\r', '\t')
  print ("charesc2", '\v', '\\', '\"', '\'')
  print ("charesc ords", map ord ['\a', '\b', '\f', '\n', '\r', '\t', '\v'])
  print ("decimal", '\65', '\097', '\0', '\127')
  print ("octal", '\o101', '\o0', '\o177', '\o12')
  print ("hex", '\x41', '\x61', '\x7e', '\x7E', '\xA')
  print ("decimal big", ord '\1234', ord '\1114111')
  print ("hex big", ord '\x10FFFF', ord '\x10ffff', ord '\o4177777')
  print ("control", map ord ['\^@', '\^A', '\^B', '\^Z'])
  print ("control2", map ord ['\^[', '\^\', '\^]', '\^^', '\^_'])
  print ("ascii names 1", map ord ['\NUL', '\SOH', '\STX', '\ETX', '\EOT', '\ENQ', '\ACK'])
  print ("ascii names 2", map ord ['\BEL', '\BS', '\HT', '\LF', '\VT', '\FF', '\CR'])
  print ("ascii names 3", map ord ['\SO', '\SI', '\DLE', '\DC1', '\DC2', '\DC3', '\DC4'])
  print ("ascii names 4", map ord ['\NAK', '\SYN', '\ETB', '\CAN', '\EM', '\SUB', '\ESC'])
  print ("ascii names 5", map ord ['\FS', '\GS', '\RS', '\US', '\SP', '\DEL'])
  print ("equalities", '\n' == '\LF', '\t' == '\HT', '\a' == '\BEL', '\x41' == 'A')
  print ("equalities2", '\^A' == '\SOH', '\^@' == '\NUL', '\SP' == ' ', '\DEL' == '\127')
  print ("double quote unescaped in char", '"' == '\"')
  print ("show escapes", show '\'', show '"', show '\\', show '\DEL', show '\200')
  -- a prime in an identifier is not a char literal
  let x' = 'x'
      f' c = c
  print ("primes vs chars", x', f' 'y', f' x')
