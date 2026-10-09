queens :: Int -> [[Int]]
queens n = go n
  where
    go 0 = [[]]
    go k = concatMap (\qs -> map (: qs) (filter (safe qs) [1 .. n])) (go (k - 1))
    safe qs q = and (zipWith (\d c -> c /= q && abs (c - q) /= d) [1 ..] qs)

main :: IO ()
main = do
  print (length (queens 6))
  putStrLn (unwords (map show (head (queens 8))))
