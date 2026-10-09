-- Haskell 2010 Report 7.1: interact / lines on input whose last line has no terminating newline
summary :: String -> String
summary s =
  unlines
    ( map describe (lines s)
        ++ [ "lines: " ++ show (length (lines s))
           , "chars: " ++ show (length s)
           , "newlines: " ++ show (length (filter (== '\n') s))
           , "ends with newline: " ++ show (last s == '\n')
           , "unlines . lines adds one: " ++ show (length (unlines (lines s)) - length s)
           ]
    )

describe :: String -> String
describe l = show (length l) ++ " [" ++ l ++ "]"

main :: IO ()
main = interact summary
