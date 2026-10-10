-- Strings as C has them: characters one after another, to a zero, or of a
-- length given. A string here is written out in UTF-8 for C's narrow
-- characters, and a character at a time for its wide ones, which are 32
-- bits. The functions with CA in their names keep only each character's
-- low eight bits.
module Foreign.C.String
  ( CString, CStringLen, peekCString, peekCStringLen, newCString, newCStringLen, withCString, withCStringLen,
    charIsRepresentable, castCharToCChar, castCCharToChar, castCharToCUChar, castCUCharToChar, castCharToCSChar, castCSCharToChar,
    peekCAString, peekCAStringLen, newCAString, newCAStringLen, withCAString, withCAStringLen,
    CWString, CWStringLen, peekCWString, peekCWStringLen, newCWString, newCWStringLen, withCWString, withCWStringLen
  )
where

import Data.Char (chr, ord)
import Foreign.Base
import Foreign.C.Types

type CString = Ptr CChar

type CStringLen = (Ptr CChar, Int)

type CWString = Ptr CWchar

type CWStringLen = (Ptr CWchar, Int)

-- Every character can be written in UTF-8.
charIsRepresentable :: Char -> IO Bool
charIsRepresentable _ = return True

castCharToCChar :: Char -> CChar
castCharToCChar c = fromIntegral (ord c `mod` 256)

castCCharToChar :: CChar -> Char
castCCharToChar c = chr (fromIntegral c `mod` 256)

castCharToCUChar :: Char -> CUChar
castCharToCUChar c = fromIntegral (ord c `mod` 256)

castCUCharToChar :: CUChar -> Char
castCUCharToChar c = chr (fromIntegral c)

castCharToCSChar :: Char -> CSChar
castCharToCSChar c = fromIntegral (ord c `mod` 256)

castCSCharToChar :: CSChar -> Char
castCSCharToChar c = chr (fromIntegral c `mod` 256)

-- A character as the bytes of its UTF-8.
encode :: Char -> [Int]
encode c
  | n < 128 = [n]
  | n < 2048 = [192 + n `div` 64, 128 + n `mod` 64]
  | n < 65536 = [224 + n `div` 4096, 128 + n `div` 64 `mod` 64, 128 + n `mod` 64]
  | otherwise = [240 + n `div` 262144, 128 + n `div` 4096 `mod` 64, 128 + n `div` 64 `mod` 64, 128 + n `mod` 64]
  where
    n = ord c

-- The characters some bytes of UTF-8 are. What is not UTF-8 is read as
-- the character that says so.
decode :: [Int] -> String
decode [] = []
decode (b : bs)
  | b < 128 = chr b : decode bs
  | b >= 192 && b < 224 = more 1 (b - 192)
  | b >= 224 && b < 240 = more 2 (b - 224)
  | b >= 240 && b < 248 = more 3 (b - 240)
  | otherwise = '\65533' : decode bs
  where
    more k first =
      let (rest, after) = splitAt k bs
       in if length rest == k && all (\x -> x >= 128 && x < 192) rest
            then chr (foldl (\acc x -> acc * 64 + (x - 128)) first rest) : decode after
            else '\65533' : decode bs

bytes :: [CChar] -> [Int]
bytes = map (\c -> fromIntegral c `mod` 256)

chars :: [Int] -> [CChar]
chars = map fromIntegral

peekCString :: CString -> IO String
peekCString p = peekArray0 0 p >>= return . decode . bytes

peekCStringLen :: CStringLen -> IO String
peekCStringLen (p, n) = peekArray n p >>= return . decode . bytes

newCString :: String -> IO CString
newCString = newArray0 0 . chars . concatMap encode

newCStringLen :: String -> IO CStringLen
newCStringLen s = let cs = chars (concatMap encode s) in newArray cs >>= \p -> return (p, length cs)

withCString :: String -> (CString -> IO a) -> IO a
withCString = withArray0 0 . chars . concatMap encode

withCStringLen :: String -> (CStringLen -> IO a) -> IO a
withCStringLen s act = withArrayLen (chars (concatMap encode s)) (\n p -> act (p, n))

peekCAString :: CString -> IO String
peekCAString p = peekArray0 0 p >>= return . map castCCharToChar

peekCAStringLen :: CStringLen -> IO String
peekCAStringLen (p, n) = peekArray n p >>= return . map castCCharToChar

newCAString :: String -> IO CString
newCAString = newArray0 0 . map castCharToCChar

newCAStringLen :: String -> IO CStringLen
newCAStringLen s = newArray (map castCharToCChar s) >>= \p -> return (p, length s)

withCAString :: String -> (CString -> IO a) -> IO a
withCAString = withArray0 0 . map castCharToCChar

withCAStringLen :: String -> (CStringLen -> IO a) -> IO a
withCAStringLen s act = withArrayLen (map castCharToCChar s) (\n p -> act (p, n))

wide :: Char -> CWchar
wide = fromIntegral . ord

narrow :: CWchar -> Char
narrow = chr . fromIntegral

peekCWString :: CWString -> IO String
peekCWString p = peekArray0 0 p >>= return . map narrow

peekCWStringLen :: CWStringLen -> IO String
peekCWStringLen (p, n) = peekArray n p >>= return . map narrow

newCWString :: String -> IO CWString
newCWString = newArray0 0 . map wide

newCWStringLen :: String -> IO CWStringLen
newCWStringLen s = newArray (map wide s) >>= \p -> return (p, length s)

withCWString :: String -> (CWString -> IO a) -> IO a
withCWString = withArray0 0 . map wide

withCWStringLen :: String -> (CWStringLen -> IO a) -> IO a
withCWStringLen s act = withArrayLen (map wide s) (\n p -> act (p, n))
