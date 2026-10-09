-- More of the libraries: arrays, bits, sized integers, complex numbers,
-- handles, exit codes, and errors of input and output caught.
import Data.Array
import Data.Bits
import Data.Complex
import Data.Int (Int8)
import Data.Word (Word16, Word8)
import System.Exit
import System.IO
import System.IO.Error

fibs :: Int -> Array Int Integer
fibs n = a
  where
    a = array (0, n) ([(0, 0), (1, 1)] ++ [(i, a ! (i - 1) + a ! (i - 2)) | i <- [2 .. n]])

main :: IO ()
main = do
  let grid = listArray ((0, 0), (1, 2)) "abcdef"
  print (fibs 30 ! 30, bounds grid, grid ! (1, 1), elems (fmap succ grid))
  print (accumArray (+) 0 (0, 3) [(i `mod` 4, i) | i <- [1 .. 10]] :: Array Int Int)
  print (listArray (1, 3) [10, 20, 30] // [(2, 99)] :: Array Int Int)
  print (5 .&. 3 :: Int, 5 .|. 3 :: Int, xor 5 3 :: Int, shiftL 1 10 :: Integer, complement 0 :: Int, popCount (255 :: Integer), testBit (5 :: Int) 2)
  print (127 + 1 :: Int8, 200 + 100 :: Word8, maxBound :: Word16, fromIntegral (300 :: Int) :: Word8, (-1) .&. 15 :: Int8)
  print ((1 :+ 2) * (3 :+ 4) :: Complex Double, magnitude (3 :+ 4 :: Complex Double), realPart (sqrt ((-4) :+ 0) :: Complex Double))
  hPutStrLn stdout "to a handle"
  h <- openFile "/tmp/powder-libraries2.txt" WriteMode
  hPutStr h "one\ntwo\n"
  hClose h
  h' <- openFile "/tmp/powder-libraries2.txt" ReadMode
  first <- hGetLine h'
  rest <- hGetContents h'
  print (first, rest)
  r <- tryIOError (readFile "/tmp/powder-no-such-file")
  putStrLn (either (\e -> if isDoesNotExistError e then "no such file" else "other") id r)
  caught <- catchIOError (ioError (userError "oops") >> return "not reached") (\e -> return ("caught: " ++ ioeGetErrorString e))
  putStrLn caught
  hPutStrLn stderr "leaving"
  exitSuccess
