-- | Calling C, for the machines written in Haskell: a function found by
-- name in what the program is linked with, or one whose address the
-- program has, called at the type the program says; and the memory C
-- sees.
--
-- A type is said by a letter for each argument and one for the result:
-- @c C s S I U L Q@ for the integers, signed and not, of 8, 16, 32 and 64
-- bits, @F@ and @D@ for float and double, @P@ for a pointer, and @V@ for
-- no result. The call is made by libffi, in @cbits/powder_foreign.c@.
module Powder.Foreign
  ( callForeign,
    loadLibraries,
    peekWhole,
    peekReal,
    pokeWhole,
    pokeReal,
    getErrno,
    setErrno,
    newCallback,
    export,
  )
where

import Control.Exception (IOException, try)
import Control.Monad
import Data.IORef
import qualified Data.Map as Map
import System.IO.Unsafe (unsafePerformIO)
import Data.Either (fromLeft, fromRight)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CChar (..), CInt (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (peekArray, withArray)
import Foreign.Ptr (FunPtr, Ptr, castFunPtrToPtr, castPtrToFunPtr, intPtrToPtr, nullPtr, ptrToIntPtr)
import Foreign.Storable (peek, poke)
import System.Environment (lookupEnv)
import System.Posix.DynamicLinker (DL (Default), RTLDFlags (RTLD_GLOBAL, RTLD_NOW), dlopen, dlsym)

foreign import ccall safe "powder_foreign_call"
  c_call :: FunPtr () -> CString -> CChar -> Ptr Int64 -> Ptr Double -> Ptr Int64 -> Ptr Double -> IO CInt

foreign import ccall unsafe "powder_errno_get" c_errno_get :: IO CInt

foreign import ccall unsafe "powder_errno_set" c_errno_set :: CInt -> IO ()

-- | Load the libraries the environment names in @POWDER_FOREIGN_LIBS@,
-- between colons, so that what is in them can be called by name. Says
-- which could not be loaded.
loadLibraries :: IO [String]
loadLibraries = do
  named <- maybe [] (filter (not . null) . splitOn ':') <$> lookupEnv "POWDER_FOREIGN_LIBS"
  fmap concat . forM named $ \file ->
    try (dlopen file [RTLD_NOW, RTLD_GLOBAL]) >>= \case
      Left (e :: IOException) -> pure ["the library " ++ file ++ " could not be loaded: " ++ show e]
      Right _ -> pure []
  where
    splitOn c s = case break (== c) s of
      (a, _ : rest) -> a : splitOn c rest
      (a, []) -> [a]

-- | Call a C function, given what its arguments are and what it gives,
-- with each argument as an integer or a double, whichever its letter is
-- of. The function is the one of a name; or for @&name@, nothing is
-- called and the address of the name is given; or for @dynamic@, the
-- function is the one whose address is the first argument. Says what is
-- wrong if it cannot.
callForeign :: String -> String -> Char -> [Either Int64 Double] -> IO (Either String (Either Int64 Double))
callForeign symbol shape result arguments = case (symbol, shape, arguments) of
  ('&' : name, _, _) -> fmap (Left . address) <$> found name
  ("dynamic", 'P' : rest, Left function : given) -> calling (castPtrToFunPtr (intPtrToPtr (fromIntegral function))) rest given
  ("dynamic", _, _) -> pure (Left "a C function called by its address needs the address first")
  _ -> found symbol >>= either (pure . Left) (\function -> calling function shape arguments)
  where
    -- A function the program made C's is found before anything of C's.
    found name =
      readIORef exported >>= \known -> case Map.lookup name known of
        Just made -> pure (Right (castPtrToFunPtr (intPtrToPtr (fromInteger made))))
        Nothing ->
          try (dlsym Default name) >>= \case
            Left (_ :: IOException) -> pure (Left ("there is no C function called " ++ name))
            Right function -> pure (Right function)
    address = fromIntegral . ptrToIntPtr . castFunPtrToPtr
    calling function kinds given =
      withCString kinds $ \kinds' ->
        withArray (map (fromLeft 0) given) $ \whole ->
          withArray (map (fromRight 0) given) $ \real ->
            alloca $ \wholeOut ->
              alloca $ \realOut -> do
                done <- c_call function kinds' (fromIntegral (fromEnum result)) whole real wholeOut realOut
                if done /= 0
                  then pure (Left ("a C function of this type cannot be called: " ++ symbol))
                  else
                    if result `elem` "DF"
                      then Right . Right <$> peek realOut
                      else if result == 'V' then pure (Right (Left 0)) else Right . Left <$> peek wholeOut

-- C calling back ----------------------------------------------------------

type Answer = [Either Int64 Double] -> IO (Either Int64 Double)

type Dispatch = Int64 -> Ptr Int64 -> Ptr Double -> Ptr Int64 -> Ptr Double -> IO ()

foreign import ccall "wrapper" makeDispatch :: Dispatch -> IO (FunPtr Dispatch)

foreign import ccall unsafe "powder_set_dispatch" c_set_dispatch :: FunPtr Dispatch -> IO ()

foreign import ccall unsafe "powder_closure_new" c_closure_new :: CString -> CChar -> Int64 -> IO (Ptr ())

-- | What answers each C function that has been made, with the letters of
-- its arguments; and the functions the program made C's, by name. They
-- are the process's, as the functions are.
answers :: IORef (Map.Map Int64 (String, Answer))
answers = unsafePerformIO (newIORef Map.empty)
{-# NOINLINE answers #-}

exported :: IORef (Map.Map String Integer)
exported = unsafePerformIO (newIORef Map.empty)
{-# NOINLINE exported #-}

-- | Make a C function of a type, given what is to answer a call of it:
-- something that takes the arguments, each an integer or a double as its
-- letter is of, and gives the result. Gives the function's address.
newCallback :: String -> Char -> Answer -> IO (Either String Integer)
newCallback shape result answer = do
  known <- readIORef answers
  -- The first function made is when C is told where calls go.
  when (Map.null known) (makeDispatch dispatch >>= c_set_dispatch)
  let which = fromIntegral (Map.size known)
  writeIORef answers (Map.insert which (shape, answer) known)
  made <- withCString shape (\shape' -> c_closure_new shape' (fromIntegral (fromEnum result)) which)
  pure (if made == nullPtr then Left "a C function of this type cannot be made" else Right (fromIntegral (ptrToIntPtr made)))
  where
    dispatch which whole real wholeOut realOut =
      readIORef answers >>= \known -> case Map.lookup which known of
        Nothing -> pure ()
        Just (kinds, respond) -> do
          wholes <- peekArray (length kinds) whole
          reals <- peekArray (length kinds) real
          respond [if kind `elem` "DF" then Right x else Left n | (kind, n, x) <- zip3 kinds wholes reals] >>= \case
            Left n -> poke wholeOut n
            Right x -> poke realOut x

-- | Make a function C's under a name: one the program calls by that name,
-- or takes the address of, is this one; and if a library that is loaded
-- has a place for its address, called @powder_export_@ and the name, the
-- address is put there, for the library's own code to call it by.
export :: String -> Integer -> IO ()
export name address = do
  modifyIORef' exported (Map.insert name address)
  try (dlsym Default ("powder_export_" ++ name)) >>= \case
    Left (_ :: IOException) -> pure ()
    Right place -> poke (castFunPtrToPtr place) (intPtrToPtr (fromInteger address) :: Ptr ())

-- Memory --------------------------------------------------------------------

-- What is at an address is said by a number: 0 to 3 for the signed
-- integers of 8, 16, 32 and 64 bits, 4 to 7 for the unsigned ones, 8 for
-- a float and 9 for a double.

at :: Integer -> Ptr a
at = intPtrToPtr . fromInteger

-- | The integer at an address.
peekWhole :: Integer -> Integer -> IO Integer
peekWhole kind address = case kind of
  0 -> toInteger <$> (peek (at address) :: IO Int8)
  1 -> toInteger <$> (peek (at address) :: IO Int16)
  2 -> toInteger <$> (peek (at address) :: IO Int32)
  3 -> toInteger <$> (peek (at address) :: IO Int64)
  4 -> toInteger <$> (peek (at address) :: IO Word8)
  5 -> toInteger <$> (peek (at address) :: IO Word16)
  6 -> toInteger <$> (peek (at address) :: IO Word32)
  _ -> toInteger <$> (peek (at address) :: IO Word64)

-- | The float or the double at an address.
peekReal :: Integer -> Integer -> IO Double
peekReal kind address = if kind == 8 then realToFrac <$> (peek (at address) :: IO Float) else peek (at address)

pokeWhole :: Integer -> Integer -> Integer -> IO ()
pokeWhole kind address value = case kind of
  0 -> poke (at address) (fromInteger value :: Int8)
  1 -> poke (at address) (fromInteger value :: Int16)
  2 -> poke (at address) (fromInteger value :: Int32)
  3 -> poke (at address) (fromInteger value :: Int64)
  4 -> poke (at address) (fromInteger value :: Word8)
  5 -> poke (at address) (fromInteger value :: Word16)
  6 -> poke (at address) (fromInteger value :: Word32)
  _ -> poke (at address) (fromInteger value :: Word64)

pokeReal :: Integer -> Integer -> Double -> IO ()
pokeReal kind address value = if kind == 8 then poke (at address) (realToFrac value :: Float) else poke (at address) value

-- | What C's @errno@ was after the program's last call, and setting what
-- the next call starts with.
getErrno :: IO Integer
getErrno = toInteger <$> c_errno_get

setErrno :: Integer -> IO ()
setErrno = c_errno_set . fromInteger
