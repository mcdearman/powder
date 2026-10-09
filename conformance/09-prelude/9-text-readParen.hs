-- Haskell 2010 Report 9 (PreludeText): readParen False p accepts optional parentheses, readParen True p requires them
module Main (main) where

-- a parser that accepts exactly the lexeme "foo" (and, unlike reads, never accepts parentheses itself)
foo :: ReadS String
foo s = [(w, rest) | (w, rest) <- lex s, w == "foo"]

-- two digits lexemes separated by a "-" lexeme
range :: ReadS (Int, Int)
range s = [((read a, read b), u) | (a, t) <- lex s, all isDigitChar a, not (null a), ("-", t') <- lex t, (b, u) <- lex t', all isDigitChar b, not (null b)]
  where
    isDigitChar c = c >= '0' && c <= '9'

main :: IO ()
main = do
  print ("optional: bare", readParen False foo "foo x")
  print ("optional: one pair", readParen False foo "(foo) x")
  print ("optional: two pairs", readParen False foo "((foo)) x")
  print ("optional: spaces", readParen False foo "  ( ( foo ) )y")
  print ("optional: unbalanced", readParen False foo "(foo x", readParen False foo "((foo) x")
  print ("optional: extra close is left over", readParen False foo "foo) x", readParen False foo "(foo)) x")
  print ("optional: wrong token", readParen False foo "bar", readParen False foo "(bar)", readParen False foo "")
  print ("mandatory: bare fails", readParen True foo "foo x")
  print ("mandatory: one pair", readParen True foo "(foo) x")
  print ("mandatory: two pairs", readParen True foo "((foo)) x")
  print ("mandatory: spaces", readParen True foo " (  foo  ) y")
  print ("mandatory: unbalanced", readParen True foo "(foo x", readParen True foo "(foo")
  print ("mandatory: wrong token", readParen True foo "(bar)", readParen True foo "()", readParen True foo "")
  print ("compound optional", readParen False range "1-5 x", readParen False range "(1 - 5) x", readParen False range "((10-20))")
  print ("compound mandatory", readParen True range "1-5", readParen True range "(1-5)", readParen True range "( 1 - 5 ) rest")
  print ("compound failures", readParen False range "1-", readParen False range "(1-5", readParen True range "(1)-5")
  print ("by precedence", map (\d -> readParen (d > 10) foo "foo") [0, 10, 11 :: Int])
  print ("by precedence parenthesised", map (\d -> readParen (d > 10) foo "(foo)") [0, 10, 11 :: Int])
  print ("nested readParen", readParen True (readParen True foo) "((foo))", readParen True (readParen True foo) "(foo)")
  print ("remaining input preserved", map snd (readParen False foo "(foo)   keep  this "))
