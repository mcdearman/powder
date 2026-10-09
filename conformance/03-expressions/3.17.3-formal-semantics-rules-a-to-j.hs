-- Haskell 2010 Report 3.17.3 (Figure 3.1): semantic identities of case expressions, rules (a)-(j), each checked as lhs == rhs
module Main (main) where

vals :: [Maybe Int]
vals = [Nothing, Just 0, Just 1, Just 7]

pairs :: [(Int, Int)]
pairs = [(0, 0), (1, 2), (5, 5)]

main :: IO ()
main = do
  -- (a) case e of { alts } = (\v -> case v of { alts }) e
  print ("a", [ (case m of { Just n -> n; Nothing -> -1 }) == (\v -> case v of { Just n -> n; Nothing -> -1 }) m | m <- vals ])
  -- (b) case v of { p1 match1; ...; pn matchn } = case v of { p1 match1; _ -> ... case v of { pn matchn; _ -> error "No match" } }
  print ("b", [ (case m of { Just 0 -> "zero"; Just _ -> "some"; Nothing -> "none" })
                == (case m of { Just 0 -> "zero"; _ -> case m of { Just _ -> "some"; _ -> case m of { Nothing -> "none"; _ -> error "No match" } } })
              | m <- vals ])
  -- (c) guards: case v of { p | g1 -> e1 | g2 -> e2 where { decls }; _ -> e' }
  --     = case e' of { y -> case v of { p -> let { decls } in case () of { () | g1 -> e1; _ -> case () of { () | g2 -> e2; _ -> y } }; _ -> y } }
  print ("c", [ (case p of { (a, b) | a > lim -> "big" | a == b -> "same" where { lim = b + 2 }; _ -> "other" })
                == (case "other" of { y -> case p of { (a, b) -> let { lim = b + 2 } in case () of { () | a > lim -> "big"; _ -> case () of { () | a == b -> "same"; _ -> y } }; _ -> y } })
              | p <- pairs ++ [(9, 1)] ])
  print ("c values", [ case p of { (a, b) | a > lim -> "big" | a == b -> "same" where { lim = b + 2 }; _ -> "other" } | p <- pairs ++ [(9, 1)] ])
  -- (d) case v of { ~p -> e; _ -> e' } = (\x1 ... xn -> e) (case v of { p -> x1 }) ... (case v of { p -> xn })
  print ("d", [ (case p of { ~(a, b) -> a - b; _ -> 0 }) == (\a b -> a - b) (case p of { (a, _) -> a }) (case p of { (_, b) -> b }) | p <- pairs ])
  print ("d on bottom", (case (undefined :: (Int, Int)) of { ~(_, _) -> 'd' }) == (\_ _ -> 'd') (case (undefined :: (Int, Int)) of { (a, _) -> a }) (case (undefined :: (Int, Int)) of { (_, b) -> b }))
  -- (e) case v of { x@p -> e; _ -> e' } = case v of { p -> (\x -> e) v; _ -> e' }
  print ("e", [ (case m of { x@(Just n) -> (x, n); _ -> (Nothing, 0) }) == (case m of { Just n -> (\x -> (x, n)) m; _ -> (Nothing, 0) }) | m <- vals ])
  -- (f) case v of { _ -> e; _ -> e' } = e
  print ("f", [ (case m of { _ -> 'e'; _ -> 'x' }) == 'e' | m <- vals ])
  print ("f on bottom", case (undefined :: Int) of { _ -> 'e'; _ -> 'x' })
  -- (g) case v of { K p1 ... pn -> e; _ -> e' }
  --     = case v of { K x1 ... xn -> case x1 of { p1 -> ... case xn of { pn -> e; _ -> e' } ...; _ -> e' }; _ -> e' }
  print ("g", [ (case p of { (0, 0) -> "origin"; _ -> "elsewhere" })
                == (case p of { (x1, x2) -> case x1 of { 0 -> case x2 of { 0 -> "origin"; _ -> "elsewhere" }; _ -> "elsewhere" } })
              | p <- pairs ])
  -- (h) case v of { k -> e; _ -> e' } = if (v == k) then e else e'
  print ("h", [ (case n of { 5 -> "five"; _ -> "not five" }) == (if n == 5 then "five" else "not five") | n <- [4, 5, 6 :: Int] ])
  print ("h char and string", [ (case s of { "ab" -> 1; _ -> 0 }) == (if s == "ab" then 1 else 0) | s <- ["ab", "a", "abc"] ])
  -- (i) case v of { x -> e; _ -> e' } = case v of { x -> e }
  print ("i", [ (case m of { x -> [x]; _ -> [] }) == (case m of { x -> [x] }) | m <- vals ])
  -- (j) case v of { x -> e } = (\x -> e) v
  print ("j", [ (case m of { x -> [x, x] }) == (\x -> [x, x]) m | m <- vals ])
  print ("j on bottom", (case (undefined :: Int) of { x -> 'j' }) == (\x -> 'j') (undefined :: Int))
