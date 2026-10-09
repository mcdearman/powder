-- Haskell 2010 Report 4.2.2: a synonym may be recursive only through a data/newtype declaration; order of declarations is free
module Main (main) where

-- Forest is used before it is declared, and is recursive through Tree
data Tree a = Node a (Forest a)

type Forest a = [Tree a]

-- chain of synonyms declared in reverse order of dependency
type C = B

type B = A

type A = Int

-- synonym recursive through a newtype
newtype Stream = Stream Unfolded

type Unfolded = (Int, Maybe Stream)

size :: Tree a -> Int
size (Node _ kids) = 1 + sizeForest kids

sizeForest :: Forest a -> Int
sizeForest ts = sum (map size ts)

labels :: Tree a -> [a]
labels (Node x kids) = x : concatMap labels kids

chained :: C -> A
chained x = x + 1

streamToList :: Stream -> [Int]
streamToList (Stream (x, Nothing)) = [x]
streamToList (Stream (x, Just rest)) = x : streamToList rest

countdown :: Int -> Stream
countdown n = Stream (n, if n <= 0 then Nothing else Just (countdown (n - 1)))

sample :: Tree Char
sample = Node 'a' [Node 'b' [], Node 'c' [Node 'd' []]]

main :: IO ()
main = do
  print ("size", size sample)
  print ("labels", labels sample)
  print ("forest", sizeForest [sample, sample])
  print ("empty forest", sizeForest ([] :: Forest Int))
  print ("chained", chained 41)
  print ("stream", streamToList (countdown 3))
