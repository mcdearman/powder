-- | Calling C: a function found by name in what the program is linked
-- with, called with numbers and giving a number.
--
-- There is no making a call of a type decided as the program runs, short
-- of a library for it, so every type a call can have is written out: up
-- to three arguments, each a double or a 64-bit integer, and a result
-- that is one of those or nothing. A narrower integer is passed in the
-- wider one, as the calling conventions have it, and the caller narrows
-- what comes back.
--
-- This module is written by a program. It is not to be edited.
module Powder.SKI.Foreign (callForeign) where

import Control.Exception (IOException, try)
import Data.Int (Int64)
import Foreign.Ptr (FunPtr, castFunPtr)
import System.Posix.DynamicLinker (DL (Default), dlsym)
foreign import ccall "dynamic" callNtoD :: FunPtr (IO Double) -> IO Double
foreign import ccall "dynamic" callNtoL :: FunPtr (IO Int64) -> IO Int64
foreign import ccall "dynamic" callNtoV :: FunPtr (IO ()) -> IO ()
foreign import ccall "dynamic" callDtoD :: FunPtr (Double -> IO Double) -> Double -> IO Double
foreign import ccall "dynamic" callDtoL :: FunPtr (Double -> IO Int64) -> Double -> IO Int64
foreign import ccall "dynamic" callDtoV :: FunPtr (Double -> IO ()) -> Double -> IO ()
foreign import ccall "dynamic" callLtoD :: FunPtr (Int64 -> IO Double) -> Int64 -> IO Double
foreign import ccall "dynamic" callLtoL :: FunPtr (Int64 -> IO Int64) -> Int64 -> IO Int64
foreign import ccall "dynamic" callLtoV :: FunPtr (Int64 -> IO ()) -> Int64 -> IO ()
foreign import ccall "dynamic" callDDtoD :: FunPtr (Double -> Double -> IO Double) -> Double -> Double -> IO Double
foreign import ccall "dynamic" callDDtoL :: FunPtr (Double -> Double -> IO Int64) -> Double -> Double -> IO Int64
foreign import ccall "dynamic" callDDtoV :: FunPtr (Double -> Double -> IO ()) -> Double -> Double -> IO ()
foreign import ccall "dynamic" callDLtoD :: FunPtr (Double -> Int64 -> IO Double) -> Double -> Int64 -> IO Double
foreign import ccall "dynamic" callDLtoL :: FunPtr (Double -> Int64 -> IO Int64) -> Double -> Int64 -> IO Int64
foreign import ccall "dynamic" callDLtoV :: FunPtr (Double -> Int64 -> IO ()) -> Double -> Int64 -> IO ()
foreign import ccall "dynamic" callLDtoD :: FunPtr (Int64 -> Double -> IO Double) -> Int64 -> Double -> IO Double
foreign import ccall "dynamic" callLDtoL :: FunPtr (Int64 -> Double -> IO Int64) -> Int64 -> Double -> IO Int64
foreign import ccall "dynamic" callLDtoV :: FunPtr (Int64 -> Double -> IO ()) -> Int64 -> Double -> IO ()
foreign import ccall "dynamic" callLLtoD :: FunPtr (Int64 -> Int64 -> IO Double) -> Int64 -> Int64 -> IO Double
foreign import ccall "dynamic" callLLtoL :: FunPtr (Int64 -> Int64 -> IO Int64) -> Int64 -> Int64 -> IO Int64
foreign import ccall "dynamic" callLLtoV :: FunPtr (Int64 -> Int64 -> IO ()) -> Int64 -> Int64 -> IO ()
foreign import ccall "dynamic" callDDDtoD :: FunPtr (Double -> Double -> Double -> IO Double) -> Double -> Double -> Double -> IO Double
foreign import ccall "dynamic" callDDDtoL :: FunPtr (Double -> Double -> Double -> IO Int64) -> Double -> Double -> Double -> IO Int64
foreign import ccall "dynamic" callDDDtoV :: FunPtr (Double -> Double -> Double -> IO ()) -> Double -> Double -> Double -> IO ()
foreign import ccall "dynamic" callDDLtoD :: FunPtr (Double -> Double -> Int64 -> IO Double) -> Double -> Double -> Int64 -> IO Double
foreign import ccall "dynamic" callDDLtoL :: FunPtr (Double -> Double -> Int64 -> IO Int64) -> Double -> Double -> Int64 -> IO Int64
foreign import ccall "dynamic" callDDLtoV :: FunPtr (Double -> Double -> Int64 -> IO ()) -> Double -> Double -> Int64 -> IO ()
foreign import ccall "dynamic" callDLDtoD :: FunPtr (Double -> Int64 -> Double -> IO Double) -> Double -> Int64 -> Double -> IO Double
foreign import ccall "dynamic" callDLDtoL :: FunPtr (Double -> Int64 -> Double -> IO Int64) -> Double -> Int64 -> Double -> IO Int64
foreign import ccall "dynamic" callDLDtoV :: FunPtr (Double -> Int64 -> Double -> IO ()) -> Double -> Int64 -> Double -> IO ()
foreign import ccall "dynamic" callDLLtoD :: FunPtr (Double -> Int64 -> Int64 -> IO Double) -> Double -> Int64 -> Int64 -> IO Double
foreign import ccall "dynamic" callDLLtoL :: FunPtr (Double -> Int64 -> Int64 -> IO Int64) -> Double -> Int64 -> Int64 -> IO Int64
foreign import ccall "dynamic" callDLLtoV :: FunPtr (Double -> Int64 -> Int64 -> IO ()) -> Double -> Int64 -> Int64 -> IO ()
foreign import ccall "dynamic" callLDDtoD :: FunPtr (Int64 -> Double -> Double -> IO Double) -> Int64 -> Double -> Double -> IO Double
foreign import ccall "dynamic" callLDDtoL :: FunPtr (Int64 -> Double -> Double -> IO Int64) -> Int64 -> Double -> Double -> IO Int64
foreign import ccall "dynamic" callLDDtoV :: FunPtr (Int64 -> Double -> Double -> IO ()) -> Int64 -> Double -> Double -> IO ()
foreign import ccall "dynamic" callLDLtoD :: FunPtr (Int64 -> Double -> Int64 -> IO Double) -> Int64 -> Double -> Int64 -> IO Double
foreign import ccall "dynamic" callLDLtoL :: FunPtr (Int64 -> Double -> Int64 -> IO Int64) -> Int64 -> Double -> Int64 -> IO Int64
foreign import ccall "dynamic" callLDLtoV :: FunPtr (Int64 -> Double -> Int64 -> IO ()) -> Int64 -> Double -> Int64 -> IO ()
foreign import ccall "dynamic" callLLDtoD :: FunPtr (Int64 -> Int64 -> Double -> IO Double) -> Int64 -> Int64 -> Double -> IO Double
foreign import ccall "dynamic" callLLDtoL :: FunPtr (Int64 -> Int64 -> Double -> IO Int64) -> Int64 -> Int64 -> Double -> IO Int64
foreign import ccall "dynamic" callLLDtoV :: FunPtr (Int64 -> Int64 -> Double -> IO ()) -> Int64 -> Int64 -> Double -> IO ()
foreign import ccall "dynamic" callLLLtoD :: FunPtr (Int64 -> Int64 -> Int64 -> IO Double) -> Int64 -> Int64 -> Int64 -> IO Double
foreign import ccall "dynamic" callLLLtoL :: FunPtr (Int64 -> Int64 -> Int64 -> IO Int64) -> Int64 -> Int64 -> Int64 -> IO Int64
foreign import ccall "dynamic" callLLLtoV :: FunPtr (Int64 -> Int64 -> Int64 -> IO ()) -> Int64 -> Int64 -> Int64 -> IO ()

-- | Call the function of a name, given what its arguments are, each a
-- double (@D@) or an integer (@L@), and whether it gives a double, an
-- integer or nothing (@V@). Says what is wrong if it cannot.
callForeign :: String -> String -> Char -> [Either Int64 Double] -> IO (Either String (Either Int64 Double))
callForeign symbol shape result arguments =
  try (dlsym Default symbol) >>= \case
    Left (_ :: IOException) -> pure (Left ("there is no C function called " ++ symbol))
    Right found -> case (shape, result, arguments) of
      ("", 'D', []) -> Right . Right <$> callNtoD (castFunPtr found)
      ("", 'L', []) -> Right . Left <$> callNtoL (castFunPtr found)
      ("", 'V', []) -> Right (Left 0) <$ callNtoV (castFunPtr found)
      ("D", 'D', [Right a0]) -> Right . Right <$> callDtoD (castFunPtr found) a0
      ("D", 'L', [Right a0]) -> Right . Left <$> callDtoL (castFunPtr found) a0
      ("D", 'V', [Right a0]) -> Right (Left 0) <$ callDtoV (castFunPtr found) a0
      ("L", 'D', [Left a0]) -> Right . Right <$> callLtoD (castFunPtr found) a0
      ("L", 'L', [Left a0]) -> Right . Left <$> callLtoL (castFunPtr found) a0
      ("L", 'V', [Left a0]) -> Right (Left 0) <$ callLtoV (castFunPtr found) a0
      ("DD", 'D', [Right a0, Right a1]) -> Right . Right <$> callDDtoD (castFunPtr found) a0 a1
      ("DD", 'L', [Right a0, Right a1]) -> Right . Left <$> callDDtoL (castFunPtr found) a0 a1
      ("DD", 'V', [Right a0, Right a1]) -> Right (Left 0) <$ callDDtoV (castFunPtr found) a0 a1
      ("DL", 'D', [Right a0, Left a1]) -> Right . Right <$> callDLtoD (castFunPtr found) a0 a1
      ("DL", 'L', [Right a0, Left a1]) -> Right . Left <$> callDLtoL (castFunPtr found) a0 a1
      ("DL", 'V', [Right a0, Left a1]) -> Right (Left 0) <$ callDLtoV (castFunPtr found) a0 a1
      ("LD", 'D', [Left a0, Right a1]) -> Right . Right <$> callLDtoD (castFunPtr found) a0 a1
      ("LD", 'L', [Left a0, Right a1]) -> Right . Left <$> callLDtoL (castFunPtr found) a0 a1
      ("LD", 'V', [Left a0, Right a1]) -> Right (Left 0) <$ callLDtoV (castFunPtr found) a0 a1
      ("LL", 'D', [Left a0, Left a1]) -> Right . Right <$> callLLtoD (castFunPtr found) a0 a1
      ("LL", 'L', [Left a0, Left a1]) -> Right . Left <$> callLLtoL (castFunPtr found) a0 a1
      ("LL", 'V', [Left a0, Left a1]) -> Right (Left 0) <$ callLLtoV (castFunPtr found) a0 a1
      ("DDD", 'D', [Right a0, Right a1, Right a2]) -> Right . Right <$> callDDDtoD (castFunPtr found) a0 a1 a2
      ("DDD", 'L', [Right a0, Right a1, Right a2]) -> Right . Left <$> callDDDtoL (castFunPtr found) a0 a1 a2
      ("DDD", 'V', [Right a0, Right a1, Right a2]) -> Right (Left 0) <$ callDDDtoV (castFunPtr found) a0 a1 a2
      ("DDL", 'D', [Right a0, Right a1, Left a2]) -> Right . Right <$> callDDLtoD (castFunPtr found) a0 a1 a2
      ("DDL", 'L', [Right a0, Right a1, Left a2]) -> Right . Left <$> callDDLtoL (castFunPtr found) a0 a1 a2
      ("DDL", 'V', [Right a0, Right a1, Left a2]) -> Right (Left 0) <$ callDDLtoV (castFunPtr found) a0 a1 a2
      ("DLD", 'D', [Right a0, Left a1, Right a2]) -> Right . Right <$> callDLDtoD (castFunPtr found) a0 a1 a2
      ("DLD", 'L', [Right a0, Left a1, Right a2]) -> Right . Left <$> callDLDtoL (castFunPtr found) a0 a1 a2
      ("DLD", 'V', [Right a0, Left a1, Right a2]) -> Right (Left 0) <$ callDLDtoV (castFunPtr found) a0 a1 a2
      ("DLL", 'D', [Right a0, Left a1, Left a2]) -> Right . Right <$> callDLLtoD (castFunPtr found) a0 a1 a2
      ("DLL", 'L', [Right a0, Left a1, Left a2]) -> Right . Left <$> callDLLtoL (castFunPtr found) a0 a1 a2
      ("DLL", 'V', [Right a0, Left a1, Left a2]) -> Right (Left 0) <$ callDLLtoV (castFunPtr found) a0 a1 a2
      ("LDD", 'D', [Left a0, Right a1, Right a2]) -> Right . Right <$> callLDDtoD (castFunPtr found) a0 a1 a2
      ("LDD", 'L', [Left a0, Right a1, Right a2]) -> Right . Left <$> callLDDtoL (castFunPtr found) a0 a1 a2
      ("LDD", 'V', [Left a0, Right a1, Right a2]) -> Right (Left 0) <$ callLDDtoV (castFunPtr found) a0 a1 a2
      ("LDL", 'D', [Left a0, Right a1, Left a2]) -> Right . Right <$> callLDLtoD (castFunPtr found) a0 a1 a2
      ("LDL", 'L', [Left a0, Right a1, Left a2]) -> Right . Left <$> callLDLtoL (castFunPtr found) a0 a1 a2
      ("LDL", 'V', [Left a0, Right a1, Left a2]) -> Right (Left 0) <$ callLDLtoV (castFunPtr found) a0 a1 a2
      ("LLD", 'D', [Left a0, Left a1, Right a2]) -> Right . Right <$> callLLDtoD (castFunPtr found) a0 a1 a2
      ("LLD", 'L', [Left a0, Left a1, Right a2]) -> Right . Left <$> callLLDtoL (castFunPtr found) a0 a1 a2
      ("LLD", 'V', [Left a0, Left a1, Right a2]) -> Right (Left 0) <$ callLLDtoV (castFunPtr found) a0 a1 a2
      ("LLL", 'D', [Left a0, Left a1, Left a2]) -> Right . Right <$> callLLLtoD (castFunPtr found) a0 a1 a2
      ("LLL", 'L', [Left a0, Left a1, Left a2]) -> Right . Left <$> callLLLtoL (castFunPtr found) a0 a1 a2
      ("LLL", 'V', [Left a0, Left a1, Left a2]) -> Right (Left 0) <$ callLLLtoV (castFunPtr found) a0 a1 a2
      _ -> pure (Left ("a C function of this type cannot be called: " ++ symbol))
