-- Handles: the three a program starts with, and files.
module System.IO
  ( Handle, IOMode (..), BufferMode (..), FilePath, stdin, stdout, stderr, openFile, withFile, hClose, hFlush,
    hIsEOF, isEOF, hSetBuffering, hGetBuffering,
    hGetChar, hGetLine, hGetContents, hPutChar, hPutStr, hPutStrLn, hPrint,
    readFile, writeFile, appendFile, putChar, putStr, putStrLn, print, getChar, getLine, getContents, interact
  )
where

import Powder.Base (IOError (..), lazily, opened)

-- A handle is a number: 0, 1 and 2 are the standard ones.
data Handle = Handle Integer

instance Eq Handle where
  Handle a == Handle b = a == b

instance Show Handle where
  show (Handle n) = "<handle " ++ show n ++ ">"

data IOMode = ReadMode | WriteMode | AppendMode deriving (Eq, Ord, Show, Enum)

stdin, stdout, stderr :: Handle
stdin = Handle 0
stdout = Handle 1
stderr = Handle 2

openFile :: FilePath -> IOMode -> IO Handle
openFile path mode = opened path (toInteger (fromEnum mode)) >>= return . Handle

hClose :: Handle -> IO ()
hClose (Handle n) = if n < 3 then return () else primFileClose n

withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile path mode act = do
  h <- openFile path mode
  result <- act h
  hClose h
  return result

hIsEOF :: Handle -> IO Bool
hIsEOF (Handle n) = primHIsEOF n

isEOF :: IO Bool
isEOF = hIsEOF stdin

data BufferMode = NoBuffering | LineBuffering | BlockBuffering (Maybe Int) deriving (Eq, Ord, Show)

-- Nothing is held back, whatever is asked.
hSetBuffering :: Handle -> BufferMode -> IO ()
hSetBuffering _ _ = return ()

hGetBuffering :: Handle -> IO BufferMode
hGetBuffering _ = return NoBuffering

-- Nothing is held back, so there is nothing to flush.
hFlush :: Handle -> IO ()
hFlush _ = return ()

-- A character's code, or -1 at the end.
code :: Handle -> IO Integer
code (Handle n) = if n == 0 then primGetChar else primHGetChar n

hGetChar :: Handle -> IO Char
hGetChar h = code h >>= \c -> if c < 0 then ioError (IOError 2 "hGetChar: end of file") else return (primIntToChar c)

hGetLine :: Handle -> IO String
hGetLine h = code h >>= \c -> if c < 0 then ioError (IOError 2 "hGetLine: end of file") else go [] c
  where
    go acc c = if c < 0 || c == 10 then return (reverse acc) else code h >>= go (primIntToChar c : acc)

hGetContents :: Handle -> IO String
hGetContents h = return (lazily (code h) (hClose h))

hPutChar :: Handle -> Char -> IO ()
hPutChar (Handle n) = primPutChar n

hPutStr :: Handle -> String -> IO ()
hPutStr h = mapM_ (hPutChar h)

hPutStrLn :: Handle -> String -> IO ()
hPutStrLn h s = hPutStr h s >> hPutChar h '\n'

hPrint :: Show a => Handle -> a -> IO ()
hPrint h x = hPutStrLn h (show x)
