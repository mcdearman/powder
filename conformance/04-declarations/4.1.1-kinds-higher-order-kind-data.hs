-- Haskell 2010 Report 4.1.1: data types of kind (* -> *) -> * and ((* -> *) -> *) -> *
module Main (main) where

-- IntIn :: (* -> *) -> *
data IntIn f = IntIn (f Int)

-- Fix :: (* -> *) -> *
newtype Fix f = In (f (Fix f))

-- ListF :: * -> * -> *
data ListF a r = NilF | ConsF a r

-- AtMaybe :: ((* -> *) -> *) -> *
data AtMaybe t = AtMaybe (t Maybe)

instance Functor (ListF a) where
  fmap _ NilF = NilF
  fmap g (ConsF a r) = ConsF a (g r)

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg (In x) = alg (fmap (cata alg) x)

fromList :: [a] -> Fix (ListF a)
fromList = foldr (\x r -> In (ConsF x r)) (In NilF)

sumAlg :: ListF Int Int -> Int
sumAlg NilF = 0
sumAlg (ConsF a r) = a + r

lenAlg :: ListF a Int -> Int
lenAlg NilF = 0
lenAlg (ConsF _ r) = 1 + r

listAlg :: ListF a [a] -> [a]
listAlg NilF = []
listAlg (ConsF a r) = a : r

showMaybeInt :: IntIn Maybe -> String
showMaybeInt (IntIn m) = show m

sumListInt :: IntIn [] -> Int
sumListInt (IntIn xs) = sum xs

unAtMaybe :: AtMaybe IntIn -> Maybe Int
unAtMaybe (AtMaybe (IntIn m)) = m

main :: IO ()
main = do
  print ("IntIn Maybe", showMaybeInt (IntIn (Just 5)))
  print ("IntIn Maybe nothing", showMaybeInt (IntIn Nothing))
  print ("IntIn []", sumListInt (IntIn [1, 2, 3]))
  print ("AtMaybe IntIn", unAtMaybe (AtMaybe (IntIn (Just 9))))
  print ("cata sum", cata sumAlg (fromList [1, 2, 3, 4]))
  print ("cata len", cata lenAlg (fromList "hello"))
  print ("cata list", cata listAlg (fromList [True, False]))
  print ("cata empty", cata lenAlg (fromList ([] :: [Int])))
