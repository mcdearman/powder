module Parts.Odd (isOdd, module Parts.Even) where

import Parts.Even

isOdd :: Int -> Bool
isOdd 0 = False
isOdd n = isEven (n - 1)
