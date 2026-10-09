-- Haskell 2010 Report 16 (Data.Char): lexLitChar and readLitChar
-- (inputs containing the empty escape \& are avoided)
module Main (main) where

import Data.Char

codes :: [(Char, String)] -> [(Int, String)]
codes rs = [(ord c, rest) | (c, rest) <- rs]

main :: IO ()
main = do
  print ("lex plain", lexLitChar "abc")
  print ("lex single", lexLitChar "z")
  print ("lex empty", lexLitChar "")
  print ("lex newline escape", lexLitChar "\\nHello")
  print ("lex tab escape", lexLitChar "\\t")
  print ("lex backslash escape", lexLitChar "\\\\x")
  print ("lex quote escapes", lexLitChar "\\\"x", lexLitChar "\\'x")
  print ("lex decimal escape", lexLitChar "\\65b")
  print ("lex hex escape", lexLitChar "\\x41z")
  print ("lex octal escape", lexLitChar "\\o101 z")
  print ("lex control escape", lexLitChar "\\^A!")
  print ("lex ascii name", lexLitChar "\\DELete")
  print ("lex SOH", lexLitChar "\\SOH")
  print ("lex space", lexLitChar " x")
  print ("read plain", readLitChar "abc")
  print ("read empty", readLitChar "")
  print ("read newline escape", readLitChar "\\nHello")
  print ("read simple escapes", map (codes . readLitChar) ["\\a", "\\b", "\\f", "\\n", "\\r", "\\t", "\\v"])
  print ("read backslash", readLitChar "\\\\x")
  print ("read quotes", readLitChar "\\\"x", readLitChar "\\'x")
  print ("read decimal escape", readLitChar "\\65b")
  print ("read decimal escape big", codes (readLitChar "\\1234 x"))
  print ("read hex escape", readLitChar "\\x41z")
  print ("read hex escape upper digits", codes (readLitChar "\\xFFg"))
  print ("read octal escape", readLitChar "\\o101 z")
  print ("read control escape", codes (readLitChar "\\^A!"), codes (readLitChar "\\^@"), codes (readLitChar "\\^_"))
  print ("read ascii names", map (codes . readLitChar) ["\\NUL", "\\ESC", "\\SP", "\\DEL", "\\US"])
  print ("read SOH wins over SO", codes (readLitChar "\\SOH"))
  print ("read SO", codes (readLitChar "\\SOx"))
  print ("read DEL then text", codes (readLitChar "\\DELete"))
  print ("read unquoted quote", readLitChar "'x")
  print ("read . showLitChar", all (\c -> readLitChar (showLitChar c "") == [(c, "")]) ['\0' .. '\300'])
  print ("lex . showLitChar", all (\c -> lexLitChar (showLitChar c "") == [(showLitChar c "", "")]) ['\0' .. '\300'])
