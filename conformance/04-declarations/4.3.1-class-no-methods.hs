-- Haskell 2010 Report 4.3.1: classes with no methods, with and without `where`, and their instances
module Main (main) where

class Marker a

class Checked a where

class (Eq a, Show a) => Key a

instance Marker Int

instance Marker Bool where

instance Marker a => Marker [a]

instance (Marker a, Marker b) => Marker (a, b)

instance Checked Char

instance Key Int

instance Key Char

instance Key a => Key [a]

tagged :: Marker a => a -> a
tagged x = x

checkedPair :: (Checked a, Marker b) => a -> b -> (a, b)
checkedPair x y = (x, y)

-- only superclass methods are used
lookupKey :: Key k => k -> [(k, v)] -> String
lookupKey k kvs = case lookup k kvs of
  Nothing -> "missing " ++ show k
  Just _ -> "found " ++ show k

main :: IO ()
main = do
  print ("tagged int", tagged (1 :: Int))
  print ("tagged list", tagged [True, False])
  print ("tagged pair", tagged (1 :: Int, [True]))
  print ("checkedPair", checkedPair 'c' (2 :: Int))
  print ("key int", lookupKey (1 :: Int) [(1, 'a')], lookupKey (2 :: Int) [(1, 'a')])
  print ("key char", lookupKey 'x' [('x', ())])
  print ("key string", lookupKey "k" [("j", True), ("k", False)])
