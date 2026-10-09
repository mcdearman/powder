-- Haskell 2010 Report 3.11: the translation identities for list comprehensions
module Main (main) where

xs :: [Int]
xs = [1, 2, 3, 4]

ps :: [(Int, Char)]
ps = [(1, 'a'), (2, 'b'), (1, 'c')]

xss :: [[Int]]
xss = [[1, 2], [], [3]]

main :: IO ()
main = do
  let n = 5 :: Int
      f = (* 3)
      p = even
  -- [e | True] = [e]
  print ("true", [n | True] == [n])
  -- [e | q] = [e | q, True]
  print ("q, True", [x * 2 | x <- xs] == [x * 2 | x <- xs, True])
  -- [e | b, Q] = if b then [e | Q] else []
  print ("guard true", [x | even (length xs), x <- xs] == (if even (length xs) then [x | x <- xs] else []))
  print ("guard false", [x | odd (length xs), x <- xs] == (if odd (length xs) then [x | x <- xs] else []))
  -- [e | p <- l, Q] = let { ok p = [e | Q]; ok _ = [] } in concatMap ok l
  print ("generator", [c | (1, c) <- ps, c /= 'z'] == (let { ok (1, c) = [c | c /= 'z']; ok _ = [] } in concatMap ok ps))
  print ("generator value", [c | (1, c) <- ps, c /= 'z'])
  -- [e | let decls, Q] = let decls in [e | Q]
  print ("let", [y + x | let y = 10, x <- xs] == (let y = 10 in [y + x | x <- xs]))
  -- fully expanded two-generator comprehension
  print ("expanded", [(x, c) | x <- xs, odd x, (_, c) <- ps]
                     == concatMap (\x -> if odd x then concatMap (\(_, c) -> [(x, c)]) ps else []) xs)
  -- familiar consequences
  print ("map", [f x | x <- xs] == map f xs)
  print ("filter", [x | x <- xs, p x] == filter p xs)
  print ("concat", [x | ys <- xss, x <- ys] == concat xss)
  print ("concatMap", [y | x <- xs, y <- [x, x]] == concatMap (\x -> [x, x]) xs)
  print ("monadic", [(x, c) | x <- xs, (_, c) <- ps] == (xs >>= \x -> ps >>= \(_, c) -> return (x, c)))
  print ("values", [f x | x <- xs, p x], [x | ys <- xss, x <- ys])
