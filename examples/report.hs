-- Things the Report pins down: lex, the escapes of characters, reading
-- and showing, tuples up to fifteen, Ix, and input read as it is asked for.
import Data.Char (digitToInt, lexLitChar, readLitChar)
import Data.Ix

main :: IO ()
main = do
  print (lex "'a' rest", lex "\"st\\\"r\\n\" x", lex "12.5e-3z", lex ">>= b", lex "(x", lex "   ", lex "x' y")
  print (lexLitChar "\\nabc", lexLitChar "\\SOHx", lexLitChar "\\x41g", readLitChar "\\^Ax", readLitChar "\\65b", readLitChar "\\o101", readLitChar "\\DELx")
  print (read "'\\n'" :: Char, read "\"a\\tb\\65\\&1\\SOH\\x41 \\\n   \\gap\"" :: String)
  putStrLn (show "\1234\&5\SO\&H\DEL\t\a\200x" ++ show '\'' ++ show '"' ++ show "'\"")
  print (read " ( 1 , 'a' , \"s\" , True , 2.5 ) " :: (Int, Char, String, Bool, Double))
  print ((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15) :: (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int))
  print (compare (1, 'a', "x", 2.5, True, 6, 7, 8) (1, 'a', "x", 2.5, True, 6, 7, 9), (minBound, maxBound) :: ((Bool, Ordering, Char), (Bool, ())))
  print (isNaN (read "NaN" :: Double), read "Infinity" :: Double, read "-3.5e2" :: Float, read "0012" :: Int)
  print (read "[LT,GT]" :: [Ordering], [LT ..], fromEnum GT, [minBound .. maxBound :: Ordering], digitToInt 'f')
  print (range ((0, 0, 0, 0), (1, 0, 1, 0)), index ((0, 0, 0, 0), (1, 0, 1, 0)) (1, 0, 0, 0), rangeSize ((1, 1), (0, 5)), rangeSize ((1, 1), (2, 5)))
  print (range (LT, GT), index (EQ, GT) GT, range ((), ()), inRange (('a', False), ('c', True)) ('b', True))
  zeros <- readFile "/dev/zero"
  print (length (takeWhile (== '\0') (take 1000 zeros)))
