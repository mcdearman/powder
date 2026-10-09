-- Haskell 2010 Report 3.17.1: every pattern form nested inside the others
module Main (main) where

data Rec = Rec { key :: Int, vals :: [Maybe (Char, String)] }
  deriving (Eq, Show)

newtype Wrap = Wrap (Either Int Rec)

deep :: Wrap -> String
deep (Wrap (Left 0)) = "left zero"
deep (Wrap (Left n)) = "left " ++ show n
deep (Wrap (Right Rec { vals = [] })) = "no vals"
deep (Wrap (Right Rec { key = 1, vals = Just ('a', s@(_ : _)) : _ })) = "key 1 a " ++ s
deep (Wrap (Right r@Rec { vals = [Nothing, ~(Just (c, _))] })) = "nothing then lazy " ++ show (key r)
deep (Wrap (Right Rec { key = k, vals = (Just (c, "")) : rest })) = "empty string " ++ [c] ++ show (k, length rest)
deep (Wrap (Right (Rec _ (_ : _)))) = "other"

quad :: ((Int, Char), [(Bool, Maybe Int)]) -> Int
quad ((n, 'x'), [(True, Just m), (False, Nothing)]) = n + m
quad ((n, _), (_, Just m) : _) = n * m
quad ((n, _), _) = n

main :: IO ()
main = do
  print ("d1", deep (Wrap (Left 0)), deep (Wrap (Left 5)))
  print ("d2", deep (Wrap (Right (Rec 9 []))))
  print ("d3", deep (Wrap (Right (Rec 1 [Just ('a', "str"), Nothing]))))
  print ("d4", deep (Wrap (Right (Rec 2 [Nothing, undefined]))))
  print ("d5", deep (Wrap (Right (Rec 3 [Just ('q', ""), Nothing, Nothing]))))
  print ("d6", deep (Wrap (Right (Rec 1 [Just ('a', "")]))))
  print ("d7", deep (Wrap (Right (Rec 4 [Just ('z', "zz")]))))
  print ("q1", quad ((1, 'x'), [(True, Just 10), (False, Nothing)]))
  print ("q2", quad ((2, 'x'), [(True, Just 10), (True, Nothing)]))
  print ("q3", quad ((3, 'y'), [(True, Nothing)]), quad ((4, 'y'), []))
  print ("mixed", case (Just [1, 2], ("ab", 'c'), [Left 1, Right "r"]) of
                    (Just (a : _), (_ : [b], 'c'), [Left 1, Right (r : _)]) -> (a, b, r)
                    _ -> (0, '?', '?'))
  print ("deep lists", case [[[1, 2], [3]], [[4]]] of { [[_, [n]], [[m]]] -> n + m; _ -> 0 })
  print ("tuple of tuples", case ((1, 2), (3, (4, 5))) of { ((a, b), (c, (d, e))) -> a + b + c + d + e })
