-- Haskell 2010 Report 4.6: a parameter of inferred kind * -> * used at Maybe, [], (,) Int, (->) Int and Either String
module Main (main) where

data App f a = A (f a)

class Sized f where
  sizeOf :: f a -> Int

instance Sized Maybe where
  sizeOf Nothing = 0
  sizeOf (Just _) = 1

instance Sized [] where
  sizeOf = length

instance Sized ((,) e) where
  sizeOf _ = 1

instance Sized (Either e) where
  sizeOf (Left _) = 0
  sizeOf (Right _) = 1

-- App f :: * -> *, so App f can itself be an instance of the constructor class
instance Sized f => Sized (App f) where
  sizeOf (A x) = sizeOf x

maybeApp :: App Maybe Int
maybeApp = A (Just 3)

listApp :: App [] Char
listApp = A "abc"

pairApp :: App ((,) Int) Bool
pairApp = A (7, True)

fnApp :: App ((->) Int) Int
fnApp = A (+ 1)

eitherApp :: App (Either String) Int
eitherApp = A (Left "no")

nestedApp :: App (App []) Bool
nestedApp = A (A [True, False])

runFn :: App ((->) r) a -> r -> a
runFn (A f) = f

unPair :: App ((,) e) a -> (e, a)
unPair (A p) = p

unEither :: App (Either e) a -> Either e a
unEither (A e) = e

main :: IO ()
main = do
  print ("Maybe", sizeOf maybeApp, sizeOf (A Nothing :: App Maybe Int))
  print ("[]", sizeOf listApp)
  print ("(,) Int", sizeOf pairApp, unPair pairApp)
  print ("(->) Int", runFn fnApp 41)
  print ("Either String", sizeOf eitherApp, unEither eitherApp)
  print ("Either String right", sizeOf (A (Right 1) :: App (Either String) Int))
  print ("nested App", sizeOf nestedApp)
  print ("function to pair", runFn (A (\x -> (x, x)) :: App ((->) Char) (Char, Char)) 'z')
