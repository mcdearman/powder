{-# LANGUAGE UnicodeSyntax #-}
-- UnicodeSyntax: the reserved operators ->, <-, => and :: may be written with one character each
module Main (main) where

twice ∷ (a → a) → a → a
twice f = f . f

total ∷ Num a ⇒ [a] → a
total xs = sum xs

classify ∷ Int → String
classify n = case n of
  0 → "zero"
  _ | even n → "even"
    | otherwise → "odd"

main ∷ IO ()
main = do
  line ← return "bound with an arrow"
  putStrLn line
  print (twice (+ 1) (5 ∷ Int), total [1, 2, 3 ∷ Int])
  print (map classify [0, 1, 2])
  print [x * 2 | x ← [1, 2, 3 ∷ Int], odd x]
  print ((\x → x + 1) (1 ∷ Int))
  -- The other spellings are as they were, and may be mixed with these.
  print (twice (\x -> x * 2) (3 :: Int))
