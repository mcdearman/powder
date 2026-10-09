-- Haskell 2010 Report 4.4.1: a type signature permits polymorphic recursion (use at a different instance in the body)
module Main (main) where

data Nested a = Flat a | Nest (Nested [a])

data Seq a = Nil | Cons a (Seq (a, a))

depth :: Nested a -> Int
depth (Flat _) = 0
depth (Nest n) = 1 + depth n

showNested :: Show a => Nested a -> String
showNested (Flat x) = show x
showNested (Nest n) = "N:" ++ showNested n

size :: Seq a -> Int
size Nil = 0
size (Cons _ rest) = 1 + 2 * size rest

toList :: Seq a -> [a]
toList Nil = []
toList (Cons x rest) = x : concatMap (\(a, b) -> [a, b]) (toList rest)

-- each recursive call is at a bigger type
grow :: Show a => Int -> a -> String
grow 0 x = show x
grow n x = grow (n - 1) (x, x)

-- mutual recursion at different instances, both with signatures
ping :: Show a => Int -> a -> String
ping 0 x = show x
ping n x = pong (n - 1) [x]

pong :: Show a => Int -> a -> String
pong 0 x = show x
pong n x = ping (n - 1) (Just x)

main :: IO ()
main = do
  print ("depth 0", depth (Flat 'a'))
  print ("depth 2", depth (Nest (Nest (Flat [[True]]))))
  print ("show nested", showNested (Nest (Nest (Flat [[1 :: Int, 2], [3]]))))
  print ("size", size (Cons 'a' (Cons ('b', 'c') (Cons (('d', 'e'), ('f', 'g')) Nil))))
  print ("toList", toList (Cons 'a' (Cons ('b', 'c') (Cons (('d', 'e'), ('f', 'g')) Nil))))
  print ("size empty", size (Nil :: Seq Int))
  print ("grow", grow 2 'x')
  print ("grow 0", grow 0 True)
  print ("ping", ping 3 (1 :: Int))
