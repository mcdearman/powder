-- Haskell 2010 Report 10.3: lines indented more than the enclosing block continue the previous item, incl. leading operators
module Main (main) where

total :: Int
total = 1
  + 2
  + 3
  * 4

cond :: Int -> Bool
cond x = x > 0
      && x < 10
      || x == 100

chain :: [Int] -> [Int]
chain = map (+ 1)
      . filter even
      . take 6

text :: String
text = "a"
    ++ "b"
    ++ concat
         [ "c"
         , "d"
         ]

app :: Int
app = negate
    $ abs
    $ 3
    - 10

main :: IO ()
main = do
  print ("total", total)
  print ("cond", cond 5, cond 50, cond 100)
  print ("chain", chain [1 ..])
  print ("text", text)
  print ("app", app)
  let v = 10
        - 3
        - 2
  print ("let continued", v)
  print ("arg continued"
    , v
      + 1)
  r <- return 1
    >>= \a -> return (a + 1)
    >>= \b -> return (b * 10)
  print ("bind chain", r :: Int)
  mapM_ print
    $ zip "ab"
    $ [1 :: Int ..]
  if v > 0
    && v < 100
    then print ("multi-line condition", True)
    else print ("multi-line condition", False)
  print ("where below", w)
  where
    w = [ x
        | x <- [1 .. 10 :: Int]
        , odd x
        ]
