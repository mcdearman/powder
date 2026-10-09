-- Haskell 2010 Report 3.6 / 10.5: optional semicolons before then/else let if/then/else align inside do blocks
module Main (main) where

classify :: Int -> IO ()
classify n = do
  if n < 0
  then putStrLn "negative"
  else if n == 0
  then putStrLn "zero"
  else putStrLn "positive"
  putStrLn "classified"

pick :: Bool -> Maybe Int
pick b = do
  x <- if b
       then Just 1
       else Nothing
  return (x + 1)

explicit :: Bool -> Int
explicit b = if b; then 1; else 2

explicit2 :: Bool -> Int
explicit2 b = if b then 1; else 2

explicit3 :: Bool -> Int
explicit3 b = if b; then 1 else 2

main :: IO ()
main = do
  classify (-1)
  classify 0
  classify 1
  print ("pick", pick True, pick False)
  print ("explicit semis", explicit True, explicit False)
  print ("explicit semi before else", explicit2 True, explicit2 False)
  print ("explicit semi before then", explicit3 True, explicit3 False)
  let v = 3 :: Int
  if v > 2
    then do
      putStrLn "nested do in then"
      putStrLn "second line"
    else return ()
  if v > 5 then putStrLn "one-line then" else putStrLn "one-line else"
  r <- if even v
    then return "even"
    else return "odd"
  putStrLn r
  if v == 3 then do
    putStrLn "then-do on the if line"
    putStrLn "still in then"
  else do
    putStrLn "else-do"
  putStrLn "done"
