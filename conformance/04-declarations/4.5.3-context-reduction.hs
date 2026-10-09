-- Haskell 2010 Report 4.5.3: context reduction: Eq [a] reduces to Eq a, Ord (a, b) to (Ord a, Ord b), and so on
module Main (main) where

-- an Eq instance that exists only for the element type: the inferred contexts below must reduce to it
data Sw = On | Off

instance Eq Sw where
  On == On = True
  Off == Off = True
  _ == _ = False

instance Show Sw where
  show On = "on"
  show Off = "off"

-- inferred: Eq a => [a] -> a -> Bool (not Eq [a])
singletonEq xs y = xs == [y]

-- inferred: Ord a => a -> a -> a -> Bool
pairLess a b c = (a, b) < (c, c)

-- inferred: Show a => a -> String
showNested x = show (Just [x])

-- inferred: (Eq a, Eq b) => a -> b -> Bool
bothEq x y = (x, [y]) == (x, [y])

-- declared with the reduced context
declared :: Eq a => [a] -> a -> Bool
declared xs y = xs == [y]

deepEq :: Eq a => [Maybe (a, [a])] -> [Maybe (a, [a])] -> Bool
deepEq = (==)

-- the Report's example: a context of the form Eq (m a) cannot be reduced and stays
liftedEq :: (Monad m, Eq (m a)) => a -> m a -> Bool
liftedEq x y = return x == y

main :: IO ()
main = do
  print ("singletonEq", singletonEq "a" 'a', singletonEq [1 :: Int] 2)
  print ("singletonEq user type", singletonEq [On] On, singletonEq [On] Off)
  print ("pairLess", pairLess 'a' 'b' 'c', pairLess (2 :: Int) 1 2, pairLess (2 :: Int) 3 2)
  print ("showNested", showNested 'x', showNested On)
  print ("bothEq", bothEq 'a' On, bothEq (1 :: Int) "s")
  print ("declared", declared [Off] Off, declared [[On]] [Off])
  print ("deepEq", deepEq [Just (On, [Off])] [Just (On, [Off])], deepEq [Nothing] [Just ('a', "")])
  print ("liftedEq maybe", liftedEq (1 :: Int) (Just 1), liftedEq 'a' Nothing)
  print ("liftedEq list", liftedEq 'a' "a", liftedEq 'a' "aa")
  print ("liftedEq user type", liftedEq On [On], liftedEq Off (Just On))
