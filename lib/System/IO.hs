-- Handles: the three a program starts with, and files. They are made in
-- Powder.Base, where the prelude's own input and output are made of them.
module System.IO
  ( fixIO, FilePath, Handle, stdin, stdout, stderr,
    withFile, openFile, IOMode (..), hClose, readFile, writeFile, appendFile,
    hFileSize, hSetFileSize, hIsEOF, isEOF, BufferMode (..), hSetBuffering, hGetBuffering, hFlush,
    hGetPosn, hSetPosn, HandlePosn, hSeek, SeekMode (..), hTell,
    hIsOpen, hIsClosed, hIsReadable, hIsWritable, hIsSeekable, hIsTerminalDevice, hSetEcho, hGetEcho, hShow,
    hWaitForInput, hReady, hGetChar, hGetLine, hLookAhead, hGetContents,
    hPutChar, hPutStr, hPutStrLn, hPrint,
    interact, putChar, putStr, putStrLn, print, getChar, getLine, getContents, readIO, readLn
  )
where

import Data.Ix
import Powder.Base
  ( BufferMode (..), Handle, HandlePosn, IOMode (..), SeekMode (..),
    fixIO, hClose, hFileSize, hFlush, hGetBuffering, hGetChar, hGetContents, hGetEcho, hGetLine, hGetPosn,
    hIsClosed, hIsEOF, hIsOpen, hIsReadable, hIsSeekable, hIsTerminalDevice, hIsWritable, hLookAhead,
    hPrint, hPutChar, hPutStr, hPutStrLn, hReady, hSeek, hSetBuffering, hSetEcho, hSetFileSize, hSetPosn,
    hShow, hTell, hWaitForInput, isEOF, openFile, stderr, stdin, stdout, withFile
  )

instance Ix IOMode where
  range (a, b) = map toEnum [fromEnum a .. fromEnum b]
  index (a, b) x = if inRange (a, b) x then fromEnum x - fromEnum a else error "Ix.index: index out of range"
  inRange (a, b) x = a <= x && x <= b

instance Ix SeekMode where
  range (a, b) = map toEnum [fromEnum a .. fromEnum b]
  index (a, b) x = if inRange (a, b) x then fromEnum x - fromEnum a else error "Ix.index: index out of range"
  inRange (a, b) x = a <= x && x <= b
