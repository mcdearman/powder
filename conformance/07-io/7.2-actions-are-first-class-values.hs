-- Haskell 2010 Report 7.2: I/O actions are ordinary values; building, storing or binding one does not perform it
say :: String -> IO ()
say s = putStrLn ("say " ++ s)

twiceIO :: IO a -> IO (a, a)
twiceIO act = do
  x <- act
  y <- act
  return (x, y)

choose :: Bool -> IO a -> IO a -> IO a
choose b t e = if b then t else e

replicateIO :: Int -> IO () -> IO ()
replicateIO 0 _ = return ()
replicateIO n act = act >> replicateIO (n - 1) act

counterAction :: IO Int
counterAction = putStrLn "counterAction runs" >> return 7

main :: IO ()
main = do
  let hello = say "hello"
      never = say "never run"
      actions = [say "first", say "second", say "third"]
      table = [("a", say "table a"), ("b", say "table b")]
  putStrLn "nothing has been said yet"
  hello
  hello
  print ("number of stored actions", length actions)
  actions !! 2
  head actions
  sequence_ (reverse actions)
  sequence_ (actions ++ actions)
  case lookup "b" table of
    Just act -> act
    Nothing -> putStrLn "missing"
  pair <- twiceIO counterAction
  print ("ran twice", pair)
  choose True (say "chosen") never
  choose False never (say "other branch")
  replicateIO 3 (say "repeated")
  let composed = hello >> say "world"
  composed
  composed
  let f = \name -> say ("to " ++ name)
  mapM_ f ["x", "y"]
  let actionFromAction = return (say "inner action") :: IO (IO ())
  inner <- actionFromAction
  putStrLn "got the inner action, not yet run"
  inner
  fst (say "from a tuple", never)
  n <- fmap (+ 1) counterAction
  print ("fmap over an action", n)
  print ("never was never run", const True never)
