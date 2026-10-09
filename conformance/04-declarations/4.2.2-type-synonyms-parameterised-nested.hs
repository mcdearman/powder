-- Haskell 2010 Report 4.2.2: type synonym declarations: plain, parameterised, nested, and of higher kind
module Main (main) where

type Name = String

type Pair a = (a, a)

type Assoc k v = [(k, v)]

type Matrix a = [[a]]

type Pred a = a -> Bool

type Table = Assoc Name (Pair Int)

type List = []

type Arrow = (->)

type Ignore a b = a

swapPair :: Pair a -> Pair a
swapPair (x, y) = (y, x)

lookupName :: Name -> Table -> Maybe (Pair Int)
lookupName = lookup

identity :: Int -> Matrix Int
identity n = [[if i == j then 1 else 0 | j <- [1 .. n]] | i <- [1 .. n]]

filterBy :: Pred a -> List a -> List a
filterBy = filter

applyArrow :: Arrow Int Int -> Int -> Int
applyArrow f x = f x

ignore :: Ignore Int Bool -> Int
ignore x = x + 1

-- a synonym and its expansion are the same type
plainString :: String
plainString = "plain"

nameLength :: Name -> Int
nameLength = length

table :: Table
table = [("a", (1, 2)), ("b", (3, 4))]

main :: IO ()
main = do
  print ("swapPair", swapPair (1 :: Int, 2))
  print ("synonym equals expansion", nameLength plainString)
  print ("pair equals tuple", swapPair ((True, False) :: (Bool, Bool)))
  print ("lookup hit", lookupName "b" table)
  print ("lookup miss", lookupName "z" table)
  print ("matrix", identity 3)
  print ("pred", filterBy even [1 .. 10 :: Int])
  print ("arrow", applyArrow (* 2) 21)
  print ("ignore", ignore 41)
  print ("annotation with synonym", ([(1, 'a')] :: Assoc Int Char))
  print ("nested annotation", (fmap swapPair (Just (1, 2)) :: Maybe (Pair Int)))
