-- Haskell 2010 Report 9 (PreludeText): lex reads a single Haskell lexeme, skipping leading white space
-- (identifiers starting with an underscore are deliberately not tested: GHC differs there)
module Main (main) where

-- split a whole string into lexemes by iterating lex
tokens :: String -> [String]
tokens s = case lex s of
  [("", _)] -> []
  [(t, rest)] -> t : tokens rest
  _ -> ["<lex failed>"]

main :: IO ()
main = do
  print ("empty", lex "")
  print ("only spaces", lex "   ", lex "\n\t ")
  print ("identifier", lex "foo bar", lex "  foo bar")
  print ("identifier with digits and primes", lex "x1 y", lex "x'y1'' z", lex "camelCase2(")
  print ("constructor", lex "Just 3", lex "True&&False")
  print ("identifier stops at symbol", lex "abc+def", lex "f.g", lex "n-1")
  print ("operator", lex "+ 3", lex "+++ 3", lex ">>= f", lex "<=5")
  print ("operator chars", lex "!@#$%&*./<=>?\\^|:-~ x")
  print ("operator stops at letter", lex "->x", lex "==y", lex ".z")
  print ("minus is an operator", lex "-5", lex "- 5")
  print ("integer", lex "123 abc", lex "0 ", lex "42)", lex "7,8")
  print ("fraction", lex "12.5 x", lex "0.001;", lex "3.14159")
  print ("exponent", lex "12.5e3 rest", lex "1e10 x", lex "7e-2;", lex "2.5E3]")
  print ("char literal", lex "'a' x", lex "'a'b", lex "' ' x")
  print ("char escapes", lex "'\\n' x", lex "'\\'' x", lex "'\\\\' x", lex "'\"' x")
  print ("string literal", lex "\"hi\" z", lex "\"\"z", lex "\"with space\" z")
  print ("string escapes", lex "\"a\\\"b\" z", lex "\"a\\nb\" z", lex "\"back\\\\slash\" z")
  print ("string containing other lexemes", lex "\"(1,2) -- 'x'\" tail")
  print ("parens", lex "(x)", lex ")", lex "() x", lex "((")
  print ("brackets", lex "[1]", lex "]", lex "[] x")
  print ("comma semicolon", lex ",y", lex ", y", lex "; z", lex ";;")
  print ("braces backquote", lex "{x}", lex "}", lex "`div` 2")
  print ("leading whitespace kinds", lex "\n\t abc", lex "\r\n42", lex "   (")
  print ("rest keeps its whitespace", lex "a   b", lex "1\n2")
  print ("tokens expr", tokens "f (x, 12) = [y + 1.5]")
  print ("tokens no spaces", tokens "let x=y*(z-1);w=\"s\"in[x,w]")
  print ("tokens literals", tokens "'a' \"b c\" 12.5e3 foo' Bar >>= ; ,")
  print ("tokens empty", tokens "", tokens "    ")
  print ("lengths", map (length . lex) ["", "x", "1", "+", "("])
