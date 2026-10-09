-- Haskell 2010 Report 41.3.1, 41.6.2 (System.IO): Eq, Ord, Show, Read, Enum and Ix instances of IOMode
-- and SeekMode
module Main (main) where

import Data.Ix
import System.IO

ioModes :: [IOMode]
ioModes = [ReadMode, WriteMode, AppendMode, ReadWriteMode]

seekModes :: [SeekMode]
seekModes = [AbsoluteSeek, RelativeSeek, SeekFromEnd]

main :: IO ()
main = do
  print ("IOMode show", ioModes)
  print ("IOMode showsPrec 11", showsPrec 11 ReadWriteMode "")
  print ("IOMode in Just", Just AppendMode)
  print ("IOMode eq", ReadMode == ReadMode, ReadMode == WriteMode, AppendMode /= ReadWriteMode)
  print ("IOMode ord", ReadMode < WriteMode, WriteMode < AppendMode, AppendMode < ReadWriteMode, compare ReadWriteMode ReadMode)
  print ("IOMode max min", maximum ioModes, minimum ioModes)
  print ("IOMode fromEnum", map fromEnum ioModes)
  print ("IOMode toEnum", map toEnum [0, 1, 2, 3] == ioModes)
  print ("IOMode enumFrom", [ReadMode ..] == ioModes, [AppendMode ..])
  print ("IOMode enumFromTo", [WriteMode .. AppendMode])
  print ("IOMode enumFromThenTo", [ReadMode, AppendMode ..], [ReadWriteMode, AppendMode .. ReadMode])
  print ("IOMode succ pred", succ ReadMode, pred ReadWriteMode)
  print ("IOMode read", (read "ReadMode" :: IOMode), (read " ( AppendMode ) " :: IOMode))
  print ("IOMode read . show", all (\m -> read (show m) == m) ioModes)
  print ("IOMode reads", (reads "WriteMode rest" :: [(IOMode, String)]), (reads "Writemode" :: [(IOMode, String)]))
  print ("IOMode range", range (ReadMode, ReadWriteMode) == ioModes, range (WriteMode, AppendMode), range (AppendMode, ReadMode))
  print ("IOMode index", map (index (ReadMode, ReadWriteMode)) ioModes, index (WriteMode, ReadWriteMode) AppendMode)
  print ("IOMode inRange", inRange (WriteMode, AppendMode) AppendMode, inRange (WriteMode, AppendMode) ReadMode)
  print ("IOMode rangeSize", rangeSize (ReadMode, ReadWriteMode))
  print ("SeekMode show", seekModes)
  print ("SeekMode showsPrec 11", showsPrec 11 SeekFromEnd "")
  print ("SeekMode eq", AbsoluteSeek == AbsoluteSeek, AbsoluteSeek == RelativeSeek, RelativeSeek /= SeekFromEnd)
  print ("SeekMode ord", AbsoluteSeek < RelativeSeek, RelativeSeek < SeekFromEnd, compare SeekFromEnd AbsoluteSeek)
  print ("SeekMode max min", maximum seekModes, minimum seekModes)
  print ("SeekMode fromEnum", map fromEnum seekModes)
  print ("SeekMode toEnum", map toEnum [0, 1, 2] == seekModes)
  print ("SeekMode enumFrom", [AbsoluteSeek ..] == seekModes, [RelativeSeek ..])
  print ("SeekMode enumFromThenTo", [SeekFromEnd, RelativeSeek .. AbsoluteSeek])
  print ("SeekMode succ pred", succ AbsoluteSeek, pred SeekFromEnd)
  print ("SeekMode read", (read "SeekFromEnd" :: SeekMode), (read "(RelativeSeek)" :: SeekMode))
  print ("SeekMode read . show", all (\m -> read (show m) == m) seekModes)
  print ("SeekMode range", range (AbsoluteSeek, SeekFromEnd) == seekModes, range (SeekFromEnd, AbsoluteSeek))
  print ("SeekMode index", map (index (AbsoluteSeek, SeekFromEnd)) seekModes)
  print ("SeekMode inRange", inRange (AbsoluteSeek, RelativeSeek) SeekFromEnd, inRange (AbsoluteSeek, RelativeSeek) RelativeSeek)
  print ("SeekMode rangeSize", rangeSize (AbsoluteSeek, SeekFromEnd))
