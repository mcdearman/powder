-- Haskell 2010 Report 3.17.3 (Figure 3.3): semantic identities for guards, rules (s)-(v)
module Main (main) where

ns :: [Int]
ns = [-3, 0, 4, 12]

ms :: [Maybe Int]
ms = [Nothing, Just 0, Just 9]

main :: IO ()
main = do
  -- (s) case () of { () | g1, ..., gn -> e; _ -> e' }
  --     = case e' of { y -> case () of { () | g1 -> ... case () of { () | gn -> e; _ -> y } ...; _ -> y } }
  print ("s", [ (case () of { () | n > 0, even n, n < 10 -> "e"; _ -> "e'" })
                == (case "e'" of { y -> case () of { () | n > 0 -> case () of { () | even n -> case () of { () | n < 10 -> "e"; _ -> y }; _ -> y }; _ -> y } })
              | n <- ns ])
  print ("s values", [ case () of { () | n > 0, even n, n < 10 -> "e"; _ -> "e'" } | n <- ns ])
  -- (t) case () of { () | e0 -> e; _ -> e' } = if e0 then e else e'
  print ("t", [ (case () of { () | n > 0 -> "pos"; _ -> "non-pos" }) == (if n > 0 then "pos" else "non-pos") | n <- ns ])
  -- (u) case () of { () | let decls -> e; _ -> e' } = let decls in e
  print ("u", [ (case () of { () | let { d = n * 2; t = d + 1 } -> d + t; _ -> 0 }) == (let { d = n * 2; t = d + 1 } in d + t) | n <- ns ])
  -- (v) case () of { () | (p <- e0) -> e; _ -> e' } = case e0 of { p -> e; _ -> e' }
  print ("v", [ (case () of { () | Just k <- m -> k + 1; _ -> 0 }) == (case m of { Just k -> k + 1; _ -> 0 }) | m <- ms ])
  print ("v values", [ case () of { () | Just k <- m -> k + 1; _ -> 0 } | m <- ms ])
  -- combinations: bindings from earlier qualifiers scope over later qualifiers and the body
  print ("combined", [ (case () of { () | Just k <- m, let { j = k * 2 }, j > 10 -> j; _ -> -1 })
                       == (case m of { Just k -> let { j = k * 2 } in if j > 10 then j else -1; _ -> -1 })
                     | m <- ms ])
  -- the fall-through value e' is shared, and is not evaluated when a guard succeeds
  print ("e' not forced", case () of { () | True -> "guard succeeded"; _ -> undefined })
  -- a failing early qualifier means later qualifiers are never evaluated
  print ("later qualifiers not forced", case () of { () | False, undefined -> "no"; _ -> "fell through" })
  print ("pattern qualifier not forced", case () of { () | False, Just _ <- (undefined :: Maybe Int) -> "no"; _ -> "fell through" })
