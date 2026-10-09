-- Haskell 2010 Report 4.1.2 / 10.5: type syntax: prefix forms (->) [] (,) (), parenthesised types, context forms
module Main (main) where

idFun :: (->) Int Int
idFun = (+ 1)

two :: (->) Int ((->) Int Int)
two a b = a - b

listOf :: [] Int
listOf = [1, 2, 3]

nestedList :: [] ([] Char)
nestedList = ["ab", "c"]

pair :: (,) Int Char
pair = (1, 'x')

triple :: (,,) Int Char Bool
triple = (1, 'y', True)

unit :: ()
unit = ()

parens :: ((Int)) -> (((Int)))
parens = id

higher :: (Int -> Int) -> Int -> Int
higher f = f . f

rightAssoc :: Int -> Int -> Int -> Int
rightAssoc a b c = a + b + c

rightAssocExplicit :: Int -> (Int -> (Int -> Int))
rightAssocExplicit = rightAssoc

maybeFun :: Maybe (Int -> Int) -> Int
maybeFun (Just f) = f 1
maybeFun Nothing = 0

applied :: Either String (Maybe [(Int, Bool)])
applied = Right (Just [(1, True)])

noContext :: () => Int -> Int
noContext = (* 2)

oneContext :: Eq a => a -> a -> Bool
oneContext = (==)

oneContextParens :: (Eq a) => a -> a -> Bool
oneContextParens = (/=)

manyContext :: (Eq a, Show a, Num a) => a -> String
manyContext x = if x == 0 then "zero" else show (x + 1)

constructorVar :: (Functor f, Eq (f Int)) => f Int -> Bool
constructorVar x = fmap (+ 0) x == x

sameVarTwice :: (Eq a, Eq a) => a -> Bool
sameVarTwice x = x == x

main :: IO ()
main = do
  print ("prefix arrow", idFun 1, two 5 3)
  print ("prefix list", listOf, nestedList)
  print ("prefix tuple", pair, triple, unit)
  print ("parens", parens 4, higher (+ 1) 0)
  print ("arrow right assoc", rightAssoc 1 2 3, rightAssocExplicit 1 2 3)
  print ("function inside type", maybeFun (Just (+ 1)), maybeFun Nothing)
  print ("nested application", applied)
  print ("contexts", noContext 4, oneContext 'a' 'a', oneContextParens 'a' 'a')
  print ("contexts2", manyContext (0 :: Int), manyContext (1.5 :: Double))
  print ("constructor variable context", constructorVar (Just 1), constructorVar [1, 2])
  print ("duplicate constraint", sameVarTwice 'q')
  print ("expression signatures", (id :: (->) Char Char) 'c', ([] :: [] ((,) Int Int)))
