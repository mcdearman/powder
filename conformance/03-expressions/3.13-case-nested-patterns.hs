-- Haskell 2010 Report 3.13: case expressions with nested patterns, tried top to bottom
module Main (main) where

data Tree = Leaf | Node Tree Int Tree
  deriving (Eq, Show)

describe :: Tree -> String
describe t = case t of
  Leaf -> "leaf"
  Node Leaf v Leaf -> "single " ++ show v
  Node (Node _ l _) v Leaf -> "left " ++ show (l, v)
  Node Leaf v (Node _ r _) -> "right " ++ show (v, r)
  Node (Node _ l _) v (Node _ r _) -> "both " ++ show (l, v, r)

classify :: [(Int, Maybe String)] -> String
classify xs = case xs of
  [] -> "empty"
  [(0, _)] -> "single zero"
  [(_, Nothing)] -> "single nothing"
  [(n, Just s)] -> "single " ++ show n ++ s
  (_, Just "x") : _ -> "starts with x"
  (a, _) : (b, _) : _ | a == b -> "same keys"
  _ : rest -> "other, rest " ++ show (length rest)

main :: IO ()
main = do
  let one = Node Leaf 1 Leaf
  print ("tree", map describe [Leaf, one, Node one 2 Leaf, Node Leaf 2 one, Node one 2 one])
  print ("list 1", classify [], classify [(0, Just "q")], classify [(1, Nothing)])
  print ("list 2", classify [(1, Just "s")], classify [(1, Just "x"), (2, Nothing)])
  print ("list 3", classify [(3, Nothing), (3, Nothing)], classify [(3, Nothing), (4, Nothing), (5, Nothing)])
  print ("tuple of lists", case ([1, 2], "ab") of { (x : _, [_, c]) -> (x, c); _ -> (0, '?') })
  print ("maybe of either", [case v of { Just (Left n) -> n; Just (Right b) -> if b then 1 else 0; Nothing -> -1 } | v <- [Just (Left 7), Just (Right True), Nothing]])
  print ("first match wins", case (1, 2) of { (_, _) -> "general"; (1, 2) -> "specific" })
  print ("literal then var", [case n of { 0 -> "zero"; 1 -> "one"; m -> "other " ++ show m } | n <- [0, 1, 2]])
  print ("string", [case s of { "" -> 0; "a" -> 1; 'a' : _ -> 2; _ -> 3 } | s <- ["", "a", "ab", "b"]])
  print ("as and nested", case [[1, 2], [3]] of { whole@(inner@(h : _) : _) -> (length whole, inner, h); _ -> (0, [], 0) })
  print ("as operand", 1 + case Just 2 of { Just n -> n; Nothing -> 0 })
  print ("result function", (case 'x' of { 'x' -> succ; _ -> pred }) 10)
