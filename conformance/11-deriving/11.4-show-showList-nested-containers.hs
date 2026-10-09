-- Haskell 2010 Report 11.4: derived types inside lists, tuples, Maybe and Either: showList gives `[A,B]` (no spaces),
-- tuple components are shown at precedence 0, Maybe/Either arguments at precedence 11
module Main (main) where

infixr 5 :+:

data E = A | B | C deriving (Show)

data P = P Int Int deriving (Show)

data I = Int :+: Int deriving (Show)

data R = R {f :: Int} deriving (Show)

main :: IO ()
main = do
  print [A, B]
  print [A, B, C, A]
  print ([] :: [E])
  print [[A], [], [B, C]]
  putStrLn (showList [A, B] "")
  putStrLn (showList [A, B] " tail")
  putStrLn (showList ([] :: [E]) "")
  putStrLn (shows [P 1 2] "" ++ " " ++ showsPrec 11 [P 1 2] "" ++ " " ++ showsPrec 11 [A] "")
  print [P 1 2, P (-3) 4]
  print [1 :+: 2, (-3) :+: 4]
  print [R 1, R (-2)]
  print (A, B)
  print (A, P 1 2, 3 :+: 4, R 5)
  print (P (-1) 2, (-3) :+: 4, R (-5))
  print ((A, B), [(C, P 0 0)], ([A], [B, C]))
  print (Just A, Just (P 1 2), Just (1 :+: 2), Just (R 1))
  print (Just [A, B], Just (A, B), Just [P 1 2], Just (Just C))
  print (Left A :: Either E P, Right (P 1 2) :: Either E P, Left (1 :+: 2) :: Either I R, Right (R 3) :: Either I R)
  print [Just A, Nothing, Just B]
  print [Left (P 1 2), Right [A, B]]
  print [(A, 1 :: Int), (B, -2)]
  print (Just (Left (Just [A, B])) :: Maybe (Either (Maybe [E]) Int))
  print [[(Just (P 1 2), [1 :+: 2])]]
  print (Just [Just [Just A]], [Just (Just (R 0))])
  print ((), A, [()], [((), B)])
  print (show [A, B] == "[A,B]", show (A, B) == "(A,B)", show [P 1 2] == "[P 1 2]")
