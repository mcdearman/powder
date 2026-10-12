-- Haskell 2010 Report 4.2.1: a constructor with a strict field works the field out when it is applied, as with ($!)
data Strict = Strict !Int Int

data Rec = Rec {forced :: !Int, left :: Int}

data Lazy = Lazy Int

main :: IO ()
main = do
  putStrLn ("lazy-field: " ++ show (case Strict 1 undefined of Strict a _ -> a))
  putStrLn ("lazy-record-field: " ++ show (forced (Rec {forced = 2, left = undefined})))
  putStrLn ("lazy-constructor: " ++ (Lazy undefined `seq` "made"))
  putStrLn ("partly-applied: " ++ (Strict undefined `seq` "a function"))
  putStrLn ("through-a-function: " ++ show (map (\f -> case f 0 of Strict a b -> a + b) [Strict 1, Strict 2]))
  Rec {forced = undefined, left = 1} `seq` putStrLn "strict-record-field: not reached"
