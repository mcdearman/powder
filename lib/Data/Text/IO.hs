{-# LANGUAGE GHC2021 #-}
-- Text read and written, as System.IO reads and writes strings.
module Data.Text.IO
  ( putStr, putStrLn, getLine, getContents, interact, readFile, writeFile, appendFile, hPutStr, hPutStrLn,
    hGetLine, hGetContents
  )
where

import Data.Text (Text, pack, unpack)
import System.IO (Handle)
import qualified System.IO as IO
import Prelude hiding (appendFile, getContents, getLine, interact, putStr, putStrLn, readFile, writeFile)
import qualified Prelude as P

putStr :: Text -> IO ()
putStr = P.putStr . unpack

putStrLn :: Text -> IO ()
putStrLn = P.putStrLn . unpack

getLine :: IO Text
getLine = fmap pack P.getLine

getContents :: IO Text
getContents = fmap pack P.getContents

interact :: (Text -> Text) -> IO ()
interact f = P.interact (unpack . f . pack)

readFile :: FilePath -> IO Text
readFile path = fmap pack (P.readFile path)

writeFile :: FilePath -> Text -> IO ()
writeFile path = P.writeFile path . unpack

appendFile :: FilePath -> Text -> IO ()
appendFile path = P.appendFile path . unpack

hPutStr :: Handle -> Text -> IO ()
hPutStr h = IO.hPutStr h . unpack

hPutStrLn :: Handle -> Text -> IO ()
hPutStrLn h = IO.hPutStrLn h . unpack

hGetLine :: Handle -> IO Text
hGetLine = fmap pack . IO.hGetLine

hGetContents :: Handle -> IO Text
hGetContents = fmap pack . IO.hGetContents
