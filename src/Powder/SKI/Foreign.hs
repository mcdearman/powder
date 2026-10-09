-- | Calling C: a function found by name in what the program is linked
-- with, called with numbers and giving a number.
--
-- There is no making a call of a type decided as the program runs, short
-- of a library for it, so every type a call can have is written out: up
-- to three arguments, each a double, a float or a 64-bit integer, and a
-- result that is one of those or nothing. A narrower integer is passed in
-- the wider one, as the calling conventions have it, and the caller
-- narrows what comes back. A float is a double to the machines, and is
-- made one on the way in and back.
--
-- This module is written by tools/foreign-tables.py. It is not to be
-- edited.
module Powder.SKI.Foreign (callForeign) where

import Control.Exception (IOException, try)
import Data.Int (Int64)
import Foreign.Ptr (FunPtr, castFunPtr)
import System.Posix.DynamicLinker (DL (Default), dlsym)

foreign import ccall "dynamic" callNtoD :: FunPtr (IO Double) -> IO Double
foreign import ccall "dynamic" callNtoL :: FunPtr (IO Int64) -> IO Int64
foreign import ccall "dynamic" callNtoF :: FunPtr (IO Float) -> IO Float
foreign import ccall "dynamic" callNtoV :: FunPtr (IO ()) -> IO ()
foreign import ccall "dynamic" callDtoD :: FunPtr (Double -> IO Double) -> Double -> IO Double
foreign import ccall "dynamic" callDtoL :: FunPtr (Double -> IO Int64) -> Double -> IO Int64
foreign import ccall "dynamic" callDtoF :: FunPtr (Double -> IO Float) -> Double -> IO Float
foreign import ccall "dynamic" callDtoV :: FunPtr (Double -> IO ()) -> Double -> IO ()
foreign import ccall "dynamic" callLtoD :: FunPtr (Int64 -> IO Double) -> Int64 -> IO Double
foreign import ccall "dynamic" callLtoL :: FunPtr (Int64 -> IO Int64) -> Int64 -> IO Int64
foreign import ccall "dynamic" callLtoF :: FunPtr (Int64 -> IO Float) -> Int64 -> IO Float
foreign import ccall "dynamic" callLtoV :: FunPtr (Int64 -> IO ()) -> Int64 -> IO ()
foreign import ccall "dynamic" callFtoD :: FunPtr (Float -> IO Double) -> Float -> IO Double
foreign import ccall "dynamic" callFtoL :: FunPtr (Float -> IO Int64) -> Float -> IO Int64
foreign import ccall "dynamic" callFtoF :: FunPtr (Float -> IO Float) -> Float -> IO Float
foreign import ccall "dynamic" callFtoV :: FunPtr (Float -> IO ()) -> Float -> IO ()
foreign import ccall "dynamic" callDDtoD :: FunPtr (Double -> Double -> IO Double) -> Double -> Double -> IO Double
foreign import ccall "dynamic" callDDtoL :: FunPtr (Double -> Double -> IO Int64) -> Double -> Double -> IO Int64
foreign import ccall "dynamic" callDDtoF :: FunPtr (Double -> Double -> IO Float) -> Double -> Double -> IO Float
foreign import ccall "dynamic" callDDtoV :: FunPtr (Double -> Double -> IO ()) -> Double -> Double -> IO ()
foreign import ccall "dynamic" callDLtoD :: FunPtr (Double -> Int64 -> IO Double) -> Double -> Int64 -> IO Double
foreign import ccall "dynamic" callDLtoL :: FunPtr (Double -> Int64 -> IO Int64) -> Double -> Int64 -> IO Int64
foreign import ccall "dynamic" callDLtoF :: FunPtr (Double -> Int64 -> IO Float) -> Double -> Int64 -> IO Float
foreign import ccall "dynamic" callDLtoV :: FunPtr (Double -> Int64 -> IO ()) -> Double -> Int64 -> IO ()
foreign import ccall "dynamic" callDFtoD :: FunPtr (Double -> Float -> IO Double) -> Double -> Float -> IO Double
foreign import ccall "dynamic" callDFtoL :: FunPtr (Double -> Float -> IO Int64) -> Double -> Float -> IO Int64
foreign import ccall "dynamic" callDFtoF :: FunPtr (Double -> Float -> IO Float) -> Double -> Float -> IO Float
foreign import ccall "dynamic" callDFtoV :: FunPtr (Double -> Float -> IO ()) -> Double -> Float -> IO ()
foreign import ccall "dynamic" callLDtoD :: FunPtr (Int64 -> Double -> IO Double) -> Int64 -> Double -> IO Double
foreign import ccall "dynamic" callLDtoL :: FunPtr (Int64 -> Double -> IO Int64) -> Int64 -> Double -> IO Int64
foreign import ccall "dynamic" callLDtoF :: FunPtr (Int64 -> Double -> IO Float) -> Int64 -> Double -> IO Float
foreign import ccall "dynamic" callLDtoV :: FunPtr (Int64 -> Double -> IO ()) -> Int64 -> Double -> IO ()
foreign import ccall "dynamic" callLLtoD :: FunPtr (Int64 -> Int64 -> IO Double) -> Int64 -> Int64 -> IO Double
foreign import ccall "dynamic" callLLtoL :: FunPtr (Int64 -> Int64 -> IO Int64) -> Int64 -> Int64 -> IO Int64
foreign import ccall "dynamic" callLLtoF :: FunPtr (Int64 -> Int64 -> IO Float) -> Int64 -> Int64 -> IO Float
foreign import ccall "dynamic" callLLtoV :: FunPtr (Int64 -> Int64 -> IO ()) -> Int64 -> Int64 -> IO ()
foreign import ccall "dynamic" callLFtoD :: FunPtr (Int64 -> Float -> IO Double) -> Int64 -> Float -> IO Double
foreign import ccall "dynamic" callLFtoL :: FunPtr (Int64 -> Float -> IO Int64) -> Int64 -> Float -> IO Int64
foreign import ccall "dynamic" callLFtoF :: FunPtr (Int64 -> Float -> IO Float) -> Int64 -> Float -> IO Float
foreign import ccall "dynamic" callLFtoV :: FunPtr (Int64 -> Float -> IO ()) -> Int64 -> Float -> IO ()
foreign import ccall "dynamic" callFDtoD :: FunPtr (Float -> Double -> IO Double) -> Float -> Double -> IO Double
foreign import ccall "dynamic" callFDtoL :: FunPtr (Float -> Double -> IO Int64) -> Float -> Double -> IO Int64
foreign import ccall "dynamic" callFDtoF :: FunPtr (Float -> Double -> IO Float) -> Float -> Double -> IO Float
foreign import ccall "dynamic" callFDtoV :: FunPtr (Float -> Double -> IO ()) -> Float -> Double -> IO ()
foreign import ccall "dynamic" callFLtoD :: FunPtr (Float -> Int64 -> IO Double) -> Float -> Int64 -> IO Double
foreign import ccall "dynamic" callFLtoL :: FunPtr (Float -> Int64 -> IO Int64) -> Float -> Int64 -> IO Int64
foreign import ccall "dynamic" callFLtoF :: FunPtr (Float -> Int64 -> IO Float) -> Float -> Int64 -> IO Float
foreign import ccall "dynamic" callFLtoV :: FunPtr (Float -> Int64 -> IO ()) -> Float -> Int64 -> IO ()
foreign import ccall "dynamic" callFFtoD :: FunPtr (Float -> Float -> IO Double) -> Float -> Float -> IO Double
foreign import ccall "dynamic" callFFtoL :: FunPtr (Float -> Float -> IO Int64) -> Float -> Float -> IO Int64
foreign import ccall "dynamic" callFFtoF :: FunPtr (Float -> Float -> IO Float) -> Float -> Float -> IO Float
foreign import ccall "dynamic" callFFtoV :: FunPtr (Float -> Float -> IO ()) -> Float -> Float -> IO ()
foreign import ccall "dynamic" callDDDtoD :: FunPtr (Double -> Double -> Double -> IO Double) -> Double -> Double -> Double -> IO Double
foreign import ccall "dynamic" callDDDtoL :: FunPtr (Double -> Double -> Double -> IO Int64) -> Double -> Double -> Double -> IO Int64
foreign import ccall "dynamic" callDDDtoF :: FunPtr (Double -> Double -> Double -> IO Float) -> Double -> Double -> Double -> IO Float
foreign import ccall "dynamic" callDDDtoV :: FunPtr (Double -> Double -> Double -> IO ()) -> Double -> Double -> Double -> IO ()
foreign import ccall "dynamic" callDDLtoD :: FunPtr (Double -> Double -> Int64 -> IO Double) -> Double -> Double -> Int64 -> IO Double
foreign import ccall "dynamic" callDDLtoL :: FunPtr (Double -> Double -> Int64 -> IO Int64) -> Double -> Double -> Int64 -> IO Int64
foreign import ccall "dynamic" callDDLtoF :: FunPtr (Double -> Double -> Int64 -> IO Float) -> Double -> Double -> Int64 -> IO Float
foreign import ccall "dynamic" callDDLtoV :: FunPtr (Double -> Double -> Int64 -> IO ()) -> Double -> Double -> Int64 -> IO ()
foreign import ccall "dynamic" callDDFtoD :: FunPtr (Double -> Double -> Float -> IO Double) -> Double -> Double -> Float -> IO Double
foreign import ccall "dynamic" callDDFtoL :: FunPtr (Double -> Double -> Float -> IO Int64) -> Double -> Double -> Float -> IO Int64
foreign import ccall "dynamic" callDDFtoF :: FunPtr (Double -> Double -> Float -> IO Float) -> Double -> Double -> Float -> IO Float
foreign import ccall "dynamic" callDDFtoV :: FunPtr (Double -> Double -> Float -> IO ()) -> Double -> Double -> Float -> IO ()
foreign import ccall "dynamic" callDLDtoD :: FunPtr (Double -> Int64 -> Double -> IO Double) -> Double -> Int64 -> Double -> IO Double
foreign import ccall "dynamic" callDLDtoL :: FunPtr (Double -> Int64 -> Double -> IO Int64) -> Double -> Int64 -> Double -> IO Int64
foreign import ccall "dynamic" callDLDtoF :: FunPtr (Double -> Int64 -> Double -> IO Float) -> Double -> Int64 -> Double -> IO Float
foreign import ccall "dynamic" callDLDtoV :: FunPtr (Double -> Int64 -> Double -> IO ()) -> Double -> Int64 -> Double -> IO ()
foreign import ccall "dynamic" callDLLtoD :: FunPtr (Double -> Int64 -> Int64 -> IO Double) -> Double -> Int64 -> Int64 -> IO Double
foreign import ccall "dynamic" callDLLtoL :: FunPtr (Double -> Int64 -> Int64 -> IO Int64) -> Double -> Int64 -> Int64 -> IO Int64
foreign import ccall "dynamic" callDLLtoF :: FunPtr (Double -> Int64 -> Int64 -> IO Float) -> Double -> Int64 -> Int64 -> IO Float
foreign import ccall "dynamic" callDLLtoV :: FunPtr (Double -> Int64 -> Int64 -> IO ()) -> Double -> Int64 -> Int64 -> IO ()
foreign import ccall "dynamic" callDLFtoD :: FunPtr (Double -> Int64 -> Float -> IO Double) -> Double -> Int64 -> Float -> IO Double
foreign import ccall "dynamic" callDLFtoL :: FunPtr (Double -> Int64 -> Float -> IO Int64) -> Double -> Int64 -> Float -> IO Int64
foreign import ccall "dynamic" callDLFtoF :: FunPtr (Double -> Int64 -> Float -> IO Float) -> Double -> Int64 -> Float -> IO Float
foreign import ccall "dynamic" callDLFtoV :: FunPtr (Double -> Int64 -> Float -> IO ()) -> Double -> Int64 -> Float -> IO ()
foreign import ccall "dynamic" callDFDtoD :: FunPtr (Double -> Float -> Double -> IO Double) -> Double -> Float -> Double -> IO Double
foreign import ccall "dynamic" callDFDtoL :: FunPtr (Double -> Float -> Double -> IO Int64) -> Double -> Float -> Double -> IO Int64
foreign import ccall "dynamic" callDFDtoF :: FunPtr (Double -> Float -> Double -> IO Float) -> Double -> Float -> Double -> IO Float
foreign import ccall "dynamic" callDFDtoV :: FunPtr (Double -> Float -> Double -> IO ()) -> Double -> Float -> Double -> IO ()
foreign import ccall "dynamic" callDFLtoD :: FunPtr (Double -> Float -> Int64 -> IO Double) -> Double -> Float -> Int64 -> IO Double
foreign import ccall "dynamic" callDFLtoL :: FunPtr (Double -> Float -> Int64 -> IO Int64) -> Double -> Float -> Int64 -> IO Int64
foreign import ccall "dynamic" callDFLtoF :: FunPtr (Double -> Float -> Int64 -> IO Float) -> Double -> Float -> Int64 -> IO Float
foreign import ccall "dynamic" callDFLtoV :: FunPtr (Double -> Float -> Int64 -> IO ()) -> Double -> Float -> Int64 -> IO ()
foreign import ccall "dynamic" callDFFtoD :: FunPtr (Double -> Float -> Float -> IO Double) -> Double -> Float -> Float -> IO Double
foreign import ccall "dynamic" callDFFtoL :: FunPtr (Double -> Float -> Float -> IO Int64) -> Double -> Float -> Float -> IO Int64
foreign import ccall "dynamic" callDFFtoF :: FunPtr (Double -> Float -> Float -> IO Float) -> Double -> Float -> Float -> IO Float
foreign import ccall "dynamic" callDFFtoV :: FunPtr (Double -> Float -> Float -> IO ()) -> Double -> Float -> Float -> IO ()
foreign import ccall "dynamic" callLDDtoD :: FunPtr (Int64 -> Double -> Double -> IO Double) -> Int64 -> Double -> Double -> IO Double
foreign import ccall "dynamic" callLDDtoL :: FunPtr (Int64 -> Double -> Double -> IO Int64) -> Int64 -> Double -> Double -> IO Int64
foreign import ccall "dynamic" callLDDtoF :: FunPtr (Int64 -> Double -> Double -> IO Float) -> Int64 -> Double -> Double -> IO Float
foreign import ccall "dynamic" callLDDtoV :: FunPtr (Int64 -> Double -> Double -> IO ()) -> Int64 -> Double -> Double -> IO ()
foreign import ccall "dynamic" callLDLtoD :: FunPtr (Int64 -> Double -> Int64 -> IO Double) -> Int64 -> Double -> Int64 -> IO Double
foreign import ccall "dynamic" callLDLtoL :: FunPtr (Int64 -> Double -> Int64 -> IO Int64) -> Int64 -> Double -> Int64 -> IO Int64
foreign import ccall "dynamic" callLDLtoF :: FunPtr (Int64 -> Double -> Int64 -> IO Float) -> Int64 -> Double -> Int64 -> IO Float
foreign import ccall "dynamic" callLDLtoV :: FunPtr (Int64 -> Double -> Int64 -> IO ()) -> Int64 -> Double -> Int64 -> IO ()
foreign import ccall "dynamic" callLDFtoD :: FunPtr (Int64 -> Double -> Float -> IO Double) -> Int64 -> Double -> Float -> IO Double
foreign import ccall "dynamic" callLDFtoL :: FunPtr (Int64 -> Double -> Float -> IO Int64) -> Int64 -> Double -> Float -> IO Int64
foreign import ccall "dynamic" callLDFtoF :: FunPtr (Int64 -> Double -> Float -> IO Float) -> Int64 -> Double -> Float -> IO Float
foreign import ccall "dynamic" callLDFtoV :: FunPtr (Int64 -> Double -> Float -> IO ()) -> Int64 -> Double -> Float -> IO ()
foreign import ccall "dynamic" callLLDtoD :: FunPtr (Int64 -> Int64 -> Double -> IO Double) -> Int64 -> Int64 -> Double -> IO Double
foreign import ccall "dynamic" callLLDtoL :: FunPtr (Int64 -> Int64 -> Double -> IO Int64) -> Int64 -> Int64 -> Double -> IO Int64
foreign import ccall "dynamic" callLLDtoF :: FunPtr (Int64 -> Int64 -> Double -> IO Float) -> Int64 -> Int64 -> Double -> IO Float
foreign import ccall "dynamic" callLLDtoV :: FunPtr (Int64 -> Int64 -> Double -> IO ()) -> Int64 -> Int64 -> Double -> IO ()
foreign import ccall "dynamic" callLLLtoD :: FunPtr (Int64 -> Int64 -> Int64 -> IO Double) -> Int64 -> Int64 -> Int64 -> IO Double
foreign import ccall "dynamic" callLLLtoL :: FunPtr (Int64 -> Int64 -> Int64 -> IO Int64) -> Int64 -> Int64 -> Int64 -> IO Int64
foreign import ccall "dynamic" callLLLtoF :: FunPtr (Int64 -> Int64 -> Int64 -> IO Float) -> Int64 -> Int64 -> Int64 -> IO Float
foreign import ccall "dynamic" callLLLtoV :: FunPtr (Int64 -> Int64 -> Int64 -> IO ()) -> Int64 -> Int64 -> Int64 -> IO ()
foreign import ccall "dynamic" callLLFtoD :: FunPtr (Int64 -> Int64 -> Float -> IO Double) -> Int64 -> Int64 -> Float -> IO Double
foreign import ccall "dynamic" callLLFtoL :: FunPtr (Int64 -> Int64 -> Float -> IO Int64) -> Int64 -> Int64 -> Float -> IO Int64
foreign import ccall "dynamic" callLLFtoF :: FunPtr (Int64 -> Int64 -> Float -> IO Float) -> Int64 -> Int64 -> Float -> IO Float
foreign import ccall "dynamic" callLLFtoV :: FunPtr (Int64 -> Int64 -> Float -> IO ()) -> Int64 -> Int64 -> Float -> IO ()
foreign import ccall "dynamic" callLFDtoD :: FunPtr (Int64 -> Float -> Double -> IO Double) -> Int64 -> Float -> Double -> IO Double
foreign import ccall "dynamic" callLFDtoL :: FunPtr (Int64 -> Float -> Double -> IO Int64) -> Int64 -> Float -> Double -> IO Int64
foreign import ccall "dynamic" callLFDtoF :: FunPtr (Int64 -> Float -> Double -> IO Float) -> Int64 -> Float -> Double -> IO Float
foreign import ccall "dynamic" callLFDtoV :: FunPtr (Int64 -> Float -> Double -> IO ()) -> Int64 -> Float -> Double -> IO ()
foreign import ccall "dynamic" callLFLtoD :: FunPtr (Int64 -> Float -> Int64 -> IO Double) -> Int64 -> Float -> Int64 -> IO Double
foreign import ccall "dynamic" callLFLtoL :: FunPtr (Int64 -> Float -> Int64 -> IO Int64) -> Int64 -> Float -> Int64 -> IO Int64
foreign import ccall "dynamic" callLFLtoF :: FunPtr (Int64 -> Float -> Int64 -> IO Float) -> Int64 -> Float -> Int64 -> IO Float
foreign import ccall "dynamic" callLFLtoV :: FunPtr (Int64 -> Float -> Int64 -> IO ()) -> Int64 -> Float -> Int64 -> IO ()
foreign import ccall "dynamic" callLFFtoD :: FunPtr (Int64 -> Float -> Float -> IO Double) -> Int64 -> Float -> Float -> IO Double
foreign import ccall "dynamic" callLFFtoL :: FunPtr (Int64 -> Float -> Float -> IO Int64) -> Int64 -> Float -> Float -> IO Int64
foreign import ccall "dynamic" callLFFtoF :: FunPtr (Int64 -> Float -> Float -> IO Float) -> Int64 -> Float -> Float -> IO Float
foreign import ccall "dynamic" callLFFtoV :: FunPtr (Int64 -> Float -> Float -> IO ()) -> Int64 -> Float -> Float -> IO ()
foreign import ccall "dynamic" callFDDtoD :: FunPtr (Float -> Double -> Double -> IO Double) -> Float -> Double -> Double -> IO Double
foreign import ccall "dynamic" callFDDtoL :: FunPtr (Float -> Double -> Double -> IO Int64) -> Float -> Double -> Double -> IO Int64
foreign import ccall "dynamic" callFDDtoF :: FunPtr (Float -> Double -> Double -> IO Float) -> Float -> Double -> Double -> IO Float
foreign import ccall "dynamic" callFDDtoV :: FunPtr (Float -> Double -> Double -> IO ()) -> Float -> Double -> Double -> IO ()
foreign import ccall "dynamic" callFDLtoD :: FunPtr (Float -> Double -> Int64 -> IO Double) -> Float -> Double -> Int64 -> IO Double
foreign import ccall "dynamic" callFDLtoL :: FunPtr (Float -> Double -> Int64 -> IO Int64) -> Float -> Double -> Int64 -> IO Int64
foreign import ccall "dynamic" callFDLtoF :: FunPtr (Float -> Double -> Int64 -> IO Float) -> Float -> Double -> Int64 -> IO Float
foreign import ccall "dynamic" callFDLtoV :: FunPtr (Float -> Double -> Int64 -> IO ()) -> Float -> Double -> Int64 -> IO ()
foreign import ccall "dynamic" callFDFtoD :: FunPtr (Float -> Double -> Float -> IO Double) -> Float -> Double -> Float -> IO Double
foreign import ccall "dynamic" callFDFtoL :: FunPtr (Float -> Double -> Float -> IO Int64) -> Float -> Double -> Float -> IO Int64
foreign import ccall "dynamic" callFDFtoF :: FunPtr (Float -> Double -> Float -> IO Float) -> Float -> Double -> Float -> IO Float
foreign import ccall "dynamic" callFDFtoV :: FunPtr (Float -> Double -> Float -> IO ()) -> Float -> Double -> Float -> IO ()
foreign import ccall "dynamic" callFLDtoD :: FunPtr (Float -> Int64 -> Double -> IO Double) -> Float -> Int64 -> Double -> IO Double
foreign import ccall "dynamic" callFLDtoL :: FunPtr (Float -> Int64 -> Double -> IO Int64) -> Float -> Int64 -> Double -> IO Int64
foreign import ccall "dynamic" callFLDtoF :: FunPtr (Float -> Int64 -> Double -> IO Float) -> Float -> Int64 -> Double -> IO Float
foreign import ccall "dynamic" callFLDtoV :: FunPtr (Float -> Int64 -> Double -> IO ()) -> Float -> Int64 -> Double -> IO ()
foreign import ccall "dynamic" callFLLtoD :: FunPtr (Float -> Int64 -> Int64 -> IO Double) -> Float -> Int64 -> Int64 -> IO Double
foreign import ccall "dynamic" callFLLtoL :: FunPtr (Float -> Int64 -> Int64 -> IO Int64) -> Float -> Int64 -> Int64 -> IO Int64
foreign import ccall "dynamic" callFLLtoF :: FunPtr (Float -> Int64 -> Int64 -> IO Float) -> Float -> Int64 -> Int64 -> IO Float
foreign import ccall "dynamic" callFLLtoV :: FunPtr (Float -> Int64 -> Int64 -> IO ()) -> Float -> Int64 -> Int64 -> IO ()
foreign import ccall "dynamic" callFLFtoD :: FunPtr (Float -> Int64 -> Float -> IO Double) -> Float -> Int64 -> Float -> IO Double
foreign import ccall "dynamic" callFLFtoL :: FunPtr (Float -> Int64 -> Float -> IO Int64) -> Float -> Int64 -> Float -> IO Int64
foreign import ccall "dynamic" callFLFtoF :: FunPtr (Float -> Int64 -> Float -> IO Float) -> Float -> Int64 -> Float -> IO Float
foreign import ccall "dynamic" callFLFtoV :: FunPtr (Float -> Int64 -> Float -> IO ()) -> Float -> Int64 -> Float -> IO ()
foreign import ccall "dynamic" callFFDtoD :: FunPtr (Float -> Float -> Double -> IO Double) -> Float -> Float -> Double -> IO Double
foreign import ccall "dynamic" callFFDtoL :: FunPtr (Float -> Float -> Double -> IO Int64) -> Float -> Float -> Double -> IO Int64
foreign import ccall "dynamic" callFFDtoF :: FunPtr (Float -> Float -> Double -> IO Float) -> Float -> Float -> Double -> IO Float
foreign import ccall "dynamic" callFFDtoV :: FunPtr (Float -> Float -> Double -> IO ()) -> Float -> Float -> Double -> IO ()
foreign import ccall "dynamic" callFFLtoD :: FunPtr (Float -> Float -> Int64 -> IO Double) -> Float -> Float -> Int64 -> IO Double
foreign import ccall "dynamic" callFFLtoL :: FunPtr (Float -> Float -> Int64 -> IO Int64) -> Float -> Float -> Int64 -> IO Int64
foreign import ccall "dynamic" callFFLtoF :: FunPtr (Float -> Float -> Int64 -> IO Float) -> Float -> Float -> Int64 -> IO Float
foreign import ccall "dynamic" callFFLtoV :: FunPtr (Float -> Float -> Int64 -> IO ()) -> Float -> Float -> Int64 -> IO ()
foreign import ccall "dynamic" callFFFtoD :: FunPtr (Float -> Float -> Float -> IO Double) -> Float -> Float -> Float -> IO Double
foreign import ccall "dynamic" callFFFtoL :: FunPtr (Float -> Float -> Float -> IO Int64) -> Float -> Float -> Float -> IO Int64
foreign import ccall "dynamic" callFFFtoF :: FunPtr (Float -> Float -> Float -> IO Float) -> Float -> Float -> Float -> IO Float
foreign import ccall "dynamic" callFFFtoV :: FunPtr (Float -> Float -> Float -> IO ()) -> Float -> Float -> Float -> IO ()

-- | Call the function of a name, given what its arguments are, each a
-- double (@D@), a float (@F@) or an integer (@L@), and whether it gives
-- one of those or nothing (@V@). Says what is wrong if it cannot.
callForeign :: String -> String -> Char -> [Either Int64 Double] -> IO (Either String (Either Int64 Double))
callForeign symbol shape result arguments =
  try (dlsym Default symbol) >>= \case
    Left (_ :: IOException) -> pure (Left ("there is no C function called " ++ symbol))
    Right found -> case (shape, result, arguments) of
      ("", 'D', []) -> Right . Right <$> callNtoD (castFunPtr found)
      ("", 'L', []) -> Right . Left <$> callNtoL (castFunPtr found)
      ("", 'F', []) -> Right . Right . realToFrac <$> callNtoF (castFunPtr found)
      ("", 'V', []) -> Right (Left 0) <$ callNtoV (castFunPtr found)
      ("D", 'D', [Right a0]) -> Right . Right <$> callDtoD (castFunPtr found) a0
      ("D", 'L', [Right a0]) -> Right . Left <$> callDtoL (castFunPtr found) a0
      ("D", 'F', [Right a0]) -> Right . Right . realToFrac <$> callDtoF (castFunPtr found) a0
      ("D", 'V', [Right a0]) -> Right (Left 0) <$ callDtoV (castFunPtr found) a0
      ("L", 'D', [Left a0]) -> Right . Right <$> callLtoD (castFunPtr found) a0
      ("L", 'L', [Left a0]) -> Right . Left <$> callLtoL (castFunPtr found) a0
      ("L", 'F', [Left a0]) -> Right . Right . realToFrac <$> callLtoF (castFunPtr found) a0
      ("L", 'V', [Left a0]) -> Right (Left 0) <$ callLtoV (castFunPtr found) a0
      ("F", 'D', [Right a0]) -> Right . Right <$> callFtoD (castFunPtr found) (realToFrac a0)
      ("F", 'L', [Right a0]) -> Right . Left <$> callFtoL (castFunPtr found) (realToFrac a0)
      ("F", 'F', [Right a0]) -> Right . Right . realToFrac <$> callFtoF (castFunPtr found) (realToFrac a0)
      ("F", 'V', [Right a0]) -> Right (Left 0) <$ callFtoV (castFunPtr found) (realToFrac a0)
      ("DD", 'D', [Right a0, Right a1]) -> Right . Right <$> callDDtoD (castFunPtr found) a0 a1
      ("DD", 'L', [Right a0, Right a1]) -> Right . Left <$> callDDtoL (castFunPtr found) a0 a1
      ("DD", 'F', [Right a0, Right a1]) -> Right . Right . realToFrac <$> callDDtoF (castFunPtr found) a0 a1
      ("DD", 'V', [Right a0, Right a1]) -> Right (Left 0) <$ callDDtoV (castFunPtr found) a0 a1
      ("DL", 'D', [Right a0, Left a1]) -> Right . Right <$> callDLtoD (castFunPtr found) a0 a1
      ("DL", 'L', [Right a0, Left a1]) -> Right . Left <$> callDLtoL (castFunPtr found) a0 a1
      ("DL", 'F', [Right a0, Left a1]) -> Right . Right . realToFrac <$> callDLtoF (castFunPtr found) a0 a1
      ("DL", 'V', [Right a0, Left a1]) -> Right (Left 0) <$ callDLtoV (castFunPtr found) a0 a1
      ("DF", 'D', [Right a0, Right a1]) -> Right . Right <$> callDFtoD (castFunPtr found) a0 (realToFrac a1)
      ("DF", 'L', [Right a0, Right a1]) -> Right . Left <$> callDFtoL (castFunPtr found) a0 (realToFrac a1)
      ("DF", 'F', [Right a0, Right a1]) -> Right . Right . realToFrac <$> callDFtoF (castFunPtr found) a0 (realToFrac a1)
      ("DF", 'V', [Right a0, Right a1]) -> Right (Left 0) <$ callDFtoV (castFunPtr found) a0 (realToFrac a1)
      ("LD", 'D', [Left a0, Right a1]) -> Right . Right <$> callLDtoD (castFunPtr found) a0 a1
      ("LD", 'L', [Left a0, Right a1]) -> Right . Left <$> callLDtoL (castFunPtr found) a0 a1
      ("LD", 'F', [Left a0, Right a1]) -> Right . Right . realToFrac <$> callLDtoF (castFunPtr found) a0 a1
      ("LD", 'V', [Left a0, Right a1]) -> Right (Left 0) <$ callLDtoV (castFunPtr found) a0 a1
      ("LL", 'D', [Left a0, Left a1]) -> Right . Right <$> callLLtoD (castFunPtr found) a0 a1
      ("LL", 'L', [Left a0, Left a1]) -> Right . Left <$> callLLtoL (castFunPtr found) a0 a1
      ("LL", 'F', [Left a0, Left a1]) -> Right . Right . realToFrac <$> callLLtoF (castFunPtr found) a0 a1
      ("LL", 'V', [Left a0, Left a1]) -> Right (Left 0) <$ callLLtoV (castFunPtr found) a0 a1
      ("LF", 'D', [Left a0, Right a1]) -> Right . Right <$> callLFtoD (castFunPtr found) a0 (realToFrac a1)
      ("LF", 'L', [Left a0, Right a1]) -> Right . Left <$> callLFtoL (castFunPtr found) a0 (realToFrac a1)
      ("LF", 'F', [Left a0, Right a1]) -> Right . Right . realToFrac <$> callLFtoF (castFunPtr found) a0 (realToFrac a1)
      ("LF", 'V', [Left a0, Right a1]) -> Right (Left 0) <$ callLFtoV (castFunPtr found) a0 (realToFrac a1)
      ("FD", 'D', [Right a0, Right a1]) -> Right . Right <$> callFDtoD (castFunPtr found) (realToFrac a0) a1
      ("FD", 'L', [Right a0, Right a1]) -> Right . Left <$> callFDtoL (castFunPtr found) (realToFrac a0) a1
      ("FD", 'F', [Right a0, Right a1]) -> Right . Right . realToFrac <$> callFDtoF (castFunPtr found) (realToFrac a0) a1
      ("FD", 'V', [Right a0, Right a1]) -> Right (Left 0) <$ callFDtoV (castFunPtr found) (realToFrac a0) a1
      ("FL", 'D', [Right a0, Left a1]) -> Right . Right <$> callFLtoD (castFunPtr found) (realToFrac a0) a1
      ("FL", 'L', [Right a0, Left a1]) -> Right . Left <$> callFLtoL (castFunPtr found) (realToFrac a0) a1
      ("FL", 'F', [Right a0, Left a1]) -> Right . Right . realToFrac <$> callFLtoF (castFunPtr found) (realToFrac a0) a1
      ("FL", 'V', [Right a0, Left a1]) -> Right (Left 0) <$ callFLtoV (castFunPtr found) (realToFrac a0) a1
      ("FF", 'D', [Right a0, Right a1]) -> Right . Right <$> callFFtoD (castFunPtr found) (realToFrac a0) (realToFrac a1)
      ("FF", 'L', [Right a0, Right a1]) -> Right . Left <$> callFFtoL (castFunPtr found) (realToFrac a0) (realToFrac a1)
      ("FF", 'F', [Right a0, Right a1]) -> Right . Right . realToFrac <$> callFFtoF (castFunPtr found) (realToFrac a0) (realToFrac a1)
      ("FF", 'V', [Right a0, Right a1]) -> Right (Left 0) <$ callFFtoV (castFunPtr found) (realToFrac a0) (realToFrac a1)
      ("DDD", 'D', [Right a0, Right a1, Right a2]) -> Right . Right <$> callDDDtoD (castFunPtr found) a0 a1 a2
      ("DDD", 'L', [Right a0, Right a1, Right a2]) -> Right . Left <$> callDDDtoL (castFunPtr found) a0 a1 a2
      ("DDD", 'F', [Right a0, Right a1, Right a2]) -> Right . Right . realToFrac <$> callDDDtoF (castFunPtr found) a0 a1 a2
      ("DDD", 'V', [Right a0, Right a1, Right a2]) -> Right (Left 0) <$ callDDDtoV (castFunPtr found) a0 a1 a2
      ("DDL", 'D', [Right a0, Right a1, Left a2]) -> Right . Right <$> callDDLtoD (castFunPtr found) a0 a1 a2
      ("DDL", 'L', [Right a0, Right a1, Left a2]) -> Right . Left <$> callDDLtoL (castFunPtr found) a0 a1 a2
      ("DDL", 'F', [Right a0, Right a1, Left a2]) -> Right . Right . realToFrac <$> callDDLtoF (castFunPtr found) a0 a1 a2
      ("DDL", 'V', [Right a0, Right a1, Left a2]) -> Right (Left 0) <$ callDDLtoV (castFunPtr found) a0 a1 a2
      ("DDF", 'D', [Right a0, Right a1, Right a2]) -> Right . Right <$> callDDFtoD (castFunPtr found) a0 a1 (realToFrac a2)
      ("DDF", 'L', [Right a0, Right a1, Right a2]) -> Right . Left <$> callDDFtoL (castFunPtr found) a0 a1 (realToFrac a2)
      ("DDF", 'F', [Right a0, Right a1, Right a2]) -> Right . Right . realToFrac <$> callDDFtoF (castFunPtr found) a0 a1 (realToFrac a2)
      ("DDF", 'V', [Right a0, Right a1, Right a2]) -> Right (Left 0) <$ callDDFtoV (castFunPtr found) a0 a1 (realToFrac a2)
      ("DLD", 'D', [Right a0, Left a1, Right a2]) -> Right . Right <$> callDLDtoD (castFunPtr found) a0 a1 a2
      ("DLD", 'L', [Right a0, Left a1, Right a2]) -> Right . Left <$> callDLDtoL (castFunPtr found) a0 a1 a2
      ("DLD", 'F', [Right a0, Left a1, Right a2]) -> Right . Right . realToFrac <$> callDLDtoF (castFunPtr found) a0 a1 a2
      ("DLD", 'V', [Right a0, Left a1, Right a2]) -> Right (Left 0) <$ callDLDtoV (castFunPtr found) a0 a1 a2
      ("DLL", 'D', [Right a0, Left a1, Left a2]) -> Right . Right <$> callDLLtoD (castFunPtr found) a0 a1 a2
      ("DLL", 'L', [Right a0, Left a1, Left a2]) -> Right . Left <$> callDLLtoL (castFunPtr found) a0 a1 a2
      ("DLL", 'F', [Right a0, Left a1, Left a2]) -> Right . Right . realToFrac <$> callDLLtoF (castFunPtr found) a0 a1 a2
      ("DLL", 'V', [Right a0, Left a1, Left a2]) -> Right (Left 0) <$ callDLLtoV (castFunPtr found) a0 a1 a2
      ("DLF", 'D', [Right a0, Left a1, Right a2]) -> Right . Right <$> callDLFtoD (castFunPtr found) a0 a1 (realToFrac a2)
      ("DLF", 'L', [Right a0, Left a1, Right a2]) -> Right . Left <$> callDLFtoL (castFunPtr found) a0 a1 (realToFrac a2)
      ("DLF", 'F', [Right a0, Left a1, Right a2]) -> Right . Right . realToFrac <$> callDLFtoF (castFunPtr found) a0 a1 (realToFrac a2)
      ("DLF", 'V', [Right a0, Left a1, Right a2]) -> Right (Left 0) <$ callDLFtoV (castFunPtr found) a0 a1 (realToFrac a2)
      ("DFD", 'D', [Right a0, Right a1, Right a2]) -> Right . Right <$> callDFDtoD (castFunPtr found) a0 (realToFrac a1) a2
      ("DFD", 'L', [Right a0, Right a1, Right a2]) -> Right . Left <$> callDFDtoL (castFunPtr found) a0 (realToFrac a1) a2
      ("DFD", 'F', [Right a0, Right a1, Right a2]) -> Right . Right . realToFrac <$> callDFDtoF (castFunPtr found) a0 (realToFrac a1) a2
      ("DFD", 'V', [Right a0, Right a1, Right a2]) -> Right (Left 0) <$ callDFDtoV (castFunPtr found) a0 (realToFrac a1) a2
      ("DFL", 'D', [Right a0, Right a1, Left a2]) -> Right . Right <$> callDFLtoD (castFunPtr found) a0 (realToFrac a1) a2
      ("DFL", 'L', [Right a0, Right a1, Left a2]) -> Right . Left <$> callDFLtoL (castFunPtr found) a0 (realToFrac a1) a2
      ("DFL", 'F', [Right a0, Right a1, Left a2]) -> Right . Right . realToFrac <$> callDFLtoF (castFunPtr found) a0 (realToFrac a1) a2
      ("DFL", 'V', [Right a0, Right a1, Left a2]) -> Right (Left 0) <$ callDFLtoV (castFunPtr found) a0 (realToFrac a1) a2
      ("DFF", 'D', [Right a0, Right a1, Right a2]) -> Right . Right <$> callDFFtoD (castFunPtr found) a0 (realToFrac a1) (realToFrac a2)
      ("DFF", 'L', [Right a0, Right a1, Right a2]) -> Right . Left <$> callDFFtoL (castFunPtr found) a0 (realToFrac a1) (realToFrac a2)
      ("DFF", 'F', [Right a0, Right a1, Right a2]) -> Right . Right . realToFrac <$> callDFFtoF (castFunPtr found) a0 (realToFrac a1) (realToFrac a2)
      ("DFF", 'V', [Right a0, Right a1, Right a2]) -> Right (Left 0) <$ callDFFtoV (castFunPtr found) a0 (realToFrac a1) (realToFrac a2)
      ("LDD", 'D', [Left a0, Right a1, Right a2]) -> Right . Right <$> callLDDtoD (castFunPtr found) a0 a1 a2
      ("LDD", 'L', [Left a0, Right a1, Right a2]) -> Right . Left <$> callLDDtoL (castFunPtr found) a0 a1 a2
      ("LDD", 'F', [Left a0, Right a1, Right a2]) -> Right . Right . realToFrac <$> callLDDtoF (castFunPtr found) a0 a1 a2
      ("LDD", 'V', [Left a0, Right a1, Right a2]) -> Right (Left 0) <$ callLDDtoV (castFunPtr found) a0 a1 a2
      ("LDL", 'D', [Left a0, Right a1, Left a2]) -> Right . Right <$> callLDLtoD (castFunPtr found) a0 a1 a2
      ("LDL", 'L', [Left a0, Right a1, Left a2]) -> Right . Left <$> callLDLtoL (castFunPtr found) a0 a1 a2
      ("LDL", 'F', [Left a0, Right a1, Left a2]) -> Right . Right . realToFrac <$> callLDLtoF (castFunPtr found) a0 a1 a2
      ("LDL", 'V', [Left a0, Right a1, Left a2]) -> Right (Left 0) <$ callLDLtoV (castFunPtr found) a0 a1 a2
      ("LDF", 'D', [Left a0, Right a1, Right a2]) -> Right . Right <$> callLDFtoD (castFunPtr found) a0 a1 (realToFrac a2)
      ("LDF", 'L', [Left a0, Right a1, Right a2]) -> Right . Left <$> callLDFtoL (castFunPtr found) a0 a1 (realToFrac a2)
      ("LDF", 'F', [Left a0, Right a1, Right a2]) -> Right . Right . realToFrac <$> callLDFtoF (castFunPtr found) a0 a1 (realToFrac a2)
      ("LDF", 'V', [Left a0, Right a1, Right a2]) -> Right (Left 0) <$ callLDFtoV (castFunPtr found) a0 a1 (realToFrac a2)
      ("LLD", 'D', [Left a0, Left a1, Right a2]) -> Right . Right <$> callLLDtoD (castFunPtr found) a0 a1 a2
      ("LLD", 'L', [Left a0, Left a1, Right a2]) -> Right . Left <$> callLLDtoL (castFunPtr found) a0 a1 a2
      ("LLD", 'F', [Left a0, Left a1, Right a2]) -> Right . Right . realToFrac <$> callLLDtoF (castFunPtr found) a0 a1 a2
      ("LLD", 'V', [Left a0, Left a1, Right a2]) -> Right (Left 0) <$ callLLDtoV (castFunPtr found) a0 a1 a2
      ("LLL", 'D', [Left a0, Left a1, Left a2]) -> Right . Right <$> callLLLtoD (castFunPtr found) a0 a1 a2
      ("LLL", 'L', [Left a0, Left a1, Left a2]) -> Right . Left <$> callLLLtoL (castFunPtr found) a0 a1 a2
      ("LLL", 'F', [Left a0, Left a1, Left a2]) -> Right . Right . realToFrac <$> callLLLtoF (castFunPtr found) a0 a1 a2
      ("LLL", 'V', [Left a0, Left a1, Left a2]) -> Right (Left 0) <$ callLLLtoV (castFunPtr found) a0 a1 a2
      ("LLF", 'D', [Left a0, Left a1, Right a2]) -> Right . Right <$> callLLFtoD (castFunPtr found) a0 a1 (realToFrac a2)
      ("LLF", 'L', [Left a0, Left a1, Right a2]) -> Right . Left <$> callLLFtoL (castFunPtr found) a0 a1 (realToFrac a2)
      ("LLF", 'F', [Left a0, Left a1, Right a2]) -> Right . Right . realToFrac <$> callLLFtoF (castFunPtr found) a0 a1 (realToFrac a2)
      ("LLF", 'V', [Left a0, Left a1, Right a2]) -> Right (Left 0) <$ callLLFtoV (castFunPtr found) a0 a1 (realToFrac a2)
      ("LFD", 'D', [Left a0, Right a1, Right a2]) -> Right . Right <$> callLFDtoD (castFunPtr found) a0 (realToFrac a1) a2
      ("LFD", 'L', [Left a0, Right a1, Right a2]) -> Right . Left <$> callLFDtoL (castFunPtr found) a0 (realToFrac a1) a2
      ("LFD", 'F', [Left a0, Right a1, Right a2]) -> Right . Right . realToFrac <$> callLFDtoF (castFunPtr found) a0 (realToFrac a1) a2
      ("LFD", 'V', [Left a0, Right a1, Right a2]) -> Right (Left 0) <$ callLFDtoV (castFunPtr found) a0 (realToFrac a1) a2
      ("LFL", 'D', [Left a0, Right a1, Left a2]) -> Right . Right <$> callLFLtoD (castFunPtr found) a0 (realToFrac a1) a2
      ("LFL", 'L', [Left a0, Right a1, Left a2]) -> Right . Left <$> callLFLtoL (castFunPtr found) a0 (realToFrac a1) a2
      ("LFL", 'F', [Left a0, Right a1, Left a2]) -> Right . Right . realToFrac <$> callLFLtoF (castFunPtr found) a0 (realToFrac a1) a2
      ("LFL", 'V', [Left a0, Right a1, Left a2]) -> Right (Left 0) <$ callLFLtoV (castFunPtr found) a0 (realToFrac a1) a2
      ("LFF", 'D', [Left a0, Right a1, Right a2]) -> Right . Right <$> callLFFtoD (castFunPtr found) a0 (realToFrac a1) (realToFrac a2)
      ("LFF", 'L', [Left a0, Right a1, Right a2]) -> Right . Left <$> callLFFtoL (castFunPtr found) a0 (realToFrac a1) (realToFrac a2)
      ("LFF", 'F', [Left a0, Right a1, Right a2]) -> Right . Right . realToFrac <$> callLFFtoF (castFunPtr found) a0 (realToFrac a1) (realToFrac a2)
      ("LFF", 'V', [Left a0, Right a1, Right a2]) -> Right (Left 0) <$ callLFFtoV (castFunPtr found) a0 (realToFrac a1) (realToFrac a2)
      ("FDD", 'D', [Right a0, Right a1, Right a2]) -> Right . Right <$> callFDDtoD (castFunPtr found) (realToFrac a0) a1 a2
      ("FDD", 'L', [Right a0, Right a1, Right a2]) -> Right . Left <$> callFDDtoL (castFunPtr found) (realToFrac a0) a1 a2
      ("FDD", 'F', [Right a0, Right a1, Right a2]) -> Right . Right . realToFrac <$> callFDDtoF (castFunPtr found) (realToFrac a0) a1 a2
      ("FDD", 'V', [Right a0, Right a1, Right a2]) -> Right (Left 0) <$ callFDDtoV (castFunPtr found) (realToFrac a0) a1 a2
      ("FDL", 'D', [Right a0, Right a1, Left a2]) -> Right . Right <$> callFDLtoD (castFunPtr found) (realToFrac a0) a1 a2
      ("FDL", 'L', [Right a0, Right a1, Left a2]) -> Right . Left <$> callFDLtoL (castFunPtr found) (realToFrac a0) a1 a2
      ("FDL", 'F', [Right a0, Right a1, Left a2]) -> Right . Right . realToFrac <$> callFDLtoF (castFunPtr found) (realToFrac a0) a1 a2
      ("FDL", 'V', [Right a0, Right a1, Left a2]) -> Right (Left 0) <$ callFDLtoV (castFunPtr found) (realToFrac a0) a1 a2
      ("FDF", 'D', [Right a0, Right a1, Right a2]) -> Right . Right <$> callFDFtoD (castFunPtr found) (realToFrac a0) a1 (realToFrac a2)
      ("FDF", 'L', [Right a0, Right a1, Right a2]) -> Right . Left <$> callFDFtoL (castFunPtr found) (realToFrac a0) a1 (realToFrac a2)
      ("FDF", 'F', [Right a0, Right a1, Right a2]) -> Right . Right . realToFrac <$> callFDFtoF (castFunPtr found) (realToFrac a0) a1 (realToFrac a2)
      ("FDF", 'V', [Right a0, Right a1, Right a2]) -> Right (Left 0) <$ callFDFtoV (castFunPtr found) (realToFrac a0) a1 (realToFrac a2)
      ("FLD", 'D', [Right a0, Left a1, Right a2]) -> Right . Right <$> callFLDtoD (castFunPtr found) (realToFrac a0) a1 a2
      ("FLD", 'L', [Right a0, Left a1, Right a2]) -> Right . Left <$> callFLDtoL (castFunPtr found) (realToFrac a0) a1 a2
      ("FLD", 'F', [Right a0, Left a1, Right a2]) -> Right . Right . realToFrac <$> callFLDtoF (castFunPtr found) (realToFrac a0) a1 a2
      ("FLD", 'V', [Right a0, Left a1, Right a2]) -> Right (Left 0) <$ callFLDtoV (castFunPtr found) (realToFrac a0) a1 a2
      ("FLL", 'D', [Right a0, Left a1, Left a2]) -> Right . Right <$> callFLLtoD (castFunPtr found) (realToFrac a0) a1 a2
      ("FLL", 'L', [Right a0, Left a1, Left a2]) -> Right . Left <$> callFLLtoL (castFunPtr found) (realToFrac a0) a1 a2
      ("FLL", 'F', [Right a0, Left a1, Left a2]) -> Right . Right . realToFrac <$> callFLLtoF (castFunPtr found) (realToFrac a0) a1 a2
      ("FLL", 'V', [Right a0, Left a1, Left a2]) -> Right (Left 0) <$ callFLLtoV (castFunPtr found) (realToFrac a0) a1 a2
      ("FLF", 'D', [Right a0, Left a1, Right a2]) -> Right . Right <$> callFLFtoD (castFunPtr found) (realToFrac a0) a1 (realToFrac a2)
      ("FLF", 'L', [Right a0, Left a1, Right a2]) -> Right . Left <$> callFLFtoL (castFunPtr found) (realToFrac a0) a1 (realToFrac a2)
      ("FLF", 'F', [Right a0, Left a1, Right a2]) -> Right . Right . realToFrac <$> callFLFtoF (castFunPtr found) (realToFrac a0) a1 (realToFrac a2)
      ("FLF", 'V', [Right a0, Left a1, Right a2]) -> Right (Left 0) <$ callFLFtoV (castFunPtr found) (realToFrac a0) a1 (realToFrac a2)
      ("FFD", 'D', [Right a0, Right a1, Right a2]) -> Right . Right <$> callFFDtoD (castFunPtr found) (realToFrac a0) (realToFrac a1) a2
      ("FFD", 'L', [Right a0, Right a1, Right a2]) -> Right . Left <$> callFFDtoL (castFunPtr found) (realToFrac a0) (realToFrac a1) a2
      ("FFD", 'F', [Right a0, Right a1, Right a2]) -> Right . Right . realToFrac <$> callFFDtoF (castFunPtr found) (realToFrac a0) (realToFrac a1) a2
      ("FFD", 'V', [Right a0, Right a1, Right a2]) -> Right (Left 0) <$ callFFDtoV (castFunPtr found) (realToFrac a0) (realToFrac a1) a2
      ("FFL", 'D', [Right a0, Right a1, Left a2]) -> Right . Right <$> callFFLtoD (castFunPtr found) (realToFrac a0) (realToFrac a1) a2
      ("FFL", 'L', [Right a0, Right a1, Left a2]) -> Right . Left <$> callFFLtoL (castFunPtr found) (realToFrac a0) (realToFrac a1) a2
      ("FFL", 'F', [Right a0, Right a1, Left a2]) -> Right . Right . realToFrac <$> callFFLtoF (castFunPtr found) (realToFrac a0) (realToFrac a1) a2
      ("FFL", 'V', [Right a0, Right a1, Left a2]) -> Right (Left 0) <$ callFFLtoV (castFunPtr found) (realToFrac a0) (realToFrac a1) a2
      ("FFF", 'D', [Right a0, Right a1, Right a2]) -> Right . Right <$> callFFFtoD (castFunPtr found) (realToFrac a0) (realToFrac a1) (realToFrac a2)
      ("FFF", 'L', [Right a0, Right a1, Right a2]) -> Right . Left <$> callFFFtoL (castFunPtr found) (realToFrac a0) (realToFrac a1) (realToFrac a2)
      ("FFF", 'F', [Right a0, Right a1, Right a2]) -> Right . Right . realToFrac <$> callFFFtoF (castFunPtr found) (realToFrac a0) (realToFrac a1) (realToFrac a2)
      ("FFF", 'V', [Right a0, Right a1, Right a2]) -> Right (Left 0) <$ callFFFtoV (castFunPtr found) (realToFrac a0) (realToFrac a1) (realToFrac a2)
      _ -> pure (Left ("a C function of this type cannot be called: " ++ symbol))
