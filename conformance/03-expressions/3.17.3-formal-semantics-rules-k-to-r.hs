-- Haskell 2010 Report 3.17.3 (Figure 3.2): semantic identities for newtypes, field labels and constructor matching, rules (k)-(r)
module Main (main) where

newtype N = N (Maybe Int)

data R = R { f1 :: Int, f2 :: Char, f3 :: Bool } | Q { f1 :: Int }
  deriving (Eq, Show)

data K = K1 Int Char | K2 Int Char | K3
  deriving (Eq, Show)

rs :: [R]
rs = [R 1 'a' True, R 1 'b' True, R 2 'a' False, Q 1]

ks :: [K]
ks = [K1 1 'a', K2 2 'b', K3]

main :: IO ()
main = do
  -- (k) case N v of { N p -> e; _ -> e' } = case v of { p -> e; _ -> e' }
  print ("k", [ (case N v of { N (Just n) -> n; _ -> -1 }) == (case v of { Just n -> n; _ -> -1 }) | v <- [Just 3, Nothing] ])
  -- (l) case _|_ of { N p -> e; _ -> e' } = case _|_ of { p -> e } (no evaluation happens for an irrefutable p)
  print ("l", (case (undefined :: N) of { N _ -> 'l'; _ -> 'x' }) == (case (undefined :: Maybe Int) of { _ -> 'l' }))
  print ("l variable", (case (undefined :: N) of { N v -> 'l' }) == (case (undefined :: Maybe Int) of { v -> 'l' }))
  -- (m) case v of { K { f1 = p1, f2 = p2, ... } -> e; _ -> e' }
  --     = case e' of { y -> case v of { K { f1 = p1 } -> case v of { K { f2 = p2, ... } -> e; _ -> y }; _ -> y } }
  print ("m", [ (case r of { R { f1 = 1, f2 = 'a' } -> "hit"; _ -> "miss" })
                == (case "miss" of { y -> case r of { R { f1 = 1 } -> case r of { R { f2 = 'a' } -> "hit"; _ -> y }; _ -> y } })
              | r <- rs ])
  print ("m values", [ case r of { R { f1 = 1, f2 = 'a' } -> "hit"; _ -> "miss" } | r <- rs ])
  -- (n) case v of { K { f = p } -> e; _ -> e' } = case v of { K p1 ... pn -> e; _ -> e' }  (pi is p at f's position, _ elsewhere)
  print ("n", [ (case r of { R { f2 = c } -> [c]; _ -> "?" }) == (case r of { R _ c _ -> [c]; _ -> "?" }) | r <- rs ])
  print ("n third", [ (case r of { R { f3 = True } -> 1; _ -> 0 }) == (case r of { R _ _ True -> 1; _ -> 0 }) | r <- rs ])
  -- (o) case v of { K {} -> e; _ -> e' } = case v of { K _ ... _ -> e; _ -> e' }
  print ("o", [ (case r of { R {} -> "R"; _ -> "Q" }) == (case r of { R _ _ _ -> "R"; _ -> "Q" }) | r <- rs ])
  print ("o non-record", [ (case k of { K1 {} -> 1; K3 {} -> 3; _ -> 0 }) == (case k of { K1 _ _ -> 1; K3 -> 3; _ -> 0 }) | k <- ks ])
  -- (p) case (K' e1 ... em) of { K x1 ... xn -> e; _ -> e' } = e'   when K and K' are distinct constructors
  print ("p", (case K2 2 'b' of { K1 x1 x2 -> show (x1, x2); _ -> "e'" }) == "e'")
  print ("p unevaluated args", case K2 undefined undefined of { K1 _ _ -> "e"; _ -> "e'" })
  -- (q) case (K e1 ... en) of { K x1 ... xn -> e; _ -> e' } = (\x1 ... xn -> e) e1 ... en
  print ("q", (case K1 1 'a' of { K1 x1 x2 -> show (x1, x2); _ -> "e'" }) == (\x1 x2 -> show (x1, x2)) 1 'a')
  print ("q unevaluated args", (case K1 undefined 'q' of { K1 _ x2 -> x2; _ -> '?' }) == (\_ x2 -> x2) (undefined :: Int) 'q')
  -- (r) case _|_ of { K x1 ... xn -> e; _ -> e' } = _|_ : not observable without diverging; the terminating neighbours are checked instead
  print ("r neighbours", case K3 of { K1 _ _ -> "K1"; K3 -> "K3"; _ -> "other" }, case (undefined :: K) of { _ -> "wildcard does not force" })
