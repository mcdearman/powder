module System.Environment (getArgs, getProgName) where

-- An argument, or the program's name for -1, a character at a time.
argument :: Integer -> IO String
argument i = go 0
  where
    go j = primArgChar i j >>= \c -> if c < 0 then return [] else go (j + 1) >>= \rest -> return (primIntToChar c : rest)

getArgs :: IO [String]
getArgs = primArgCount >>= \n -> mapM argument (enumFromTo 0 (n - 1))

getProgName :: IO String
getProgName = argument (negate 1)
