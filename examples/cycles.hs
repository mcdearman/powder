-- Values that refer to each other in a circle, made and dropped many
-- times over: local functions that call each other, and a ring of nodes.
-- A machine that only counts references never frees them.
parity :: Int -> (Bool, Bool)
parity n = (isEven n, isOdd n)
  where
    isEven k = if k == 0 then True else isOdd (k - 1)
    isOdd k = if k == 0 then False else isEven (k - 1)

evens :: Int -> Int -> Int
evens acc n = if n == 0 then acc else evens (if fst (parity (n `mod` 7)) then acc + 1 else acc) (n - 1)

data Node = Node Int Node

ring :: Int -> Int
ring n = let a = Node n b; b = Node (n + 1) c; c = Node (n + 2) a in case b of Node x (Node y (Node z _)) -> x + y + z

rings :: Int -> Int -> Int
rings acc n = if n == 0 then acc else rings (acc + ring n `mod` 5) (n - 1)

main :: IO ()
main = do
  print (evens 0 20000)
  print (rings 0 20000)
  let ones = 1 : ones :: [Int]
  print (take 5 ones)
