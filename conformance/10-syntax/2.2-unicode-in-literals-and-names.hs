-- Haskell 2010 Report 2.1 / 2.2 / 2.6: Unicode in char/string literals, comments, identifiers (uniSmall/uniLarge) and symbols (uniSymbol)
-- (this file intentionally contains non-ASCII characters, UTF-8 encoded)
module Main (main) where

import Data.Char (ord, chr)

-- a comment with non-ASCII text: naïve café Ελληνικά 日本語 ∀∃

data Größe = Klein | Groß deriving Eq

α :: Int
α = 1

βγ' :: Int
βγ' = 2

naïve :: Int -> Int
naïve x = x + α

infixl 6 ⊕

(⊕) :: Int -> Int -> Int
a ⊕ b = a + b

(∘) :: (b -> c) -> (a -> b) -> a -> c
f ∘ g = \x -> f (g x)

main :: IO ()
main = do
  print ("ord of literals", map ord "aé", ord 'λ', ord '日')
  print ("ord of astral literal", ord '𝄞')
  print ("length counts code points", length "naïve", length "日本語", length "𝄞")
  print ("literal equals escape", 'é' == '\233', 'λ' == '\955', "日" == "\26085")
  print ("literal equals hex escape", '𝄞' == '\x1D11E', 'é' == chr 233)
  print ("show escapes non-ASCII", show 'é', show "λx", show "日本")
  print ("unicode identifiers", α, βγ', naïve 1)
  print ("unicode operator", 1 ⊕ 2 ⊕ 3, ((+ 1) ∘ (* 2)) 5)
  print ("unicode constructors (not shown: output stays ASCII)", Klein == Groß, Groß == Groß)
  putStrLn (map (chr . ord) "ASCII only output from non-ASCII source")
