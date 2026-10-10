-- What the Foreign libraries are made of: addresses, what can be kept at
-- one, and the memory C gives out. The modules the Report names are
-- selections from this one.
--
-- This module's instances are written by a program, from the table of C's
-- types and their sizes on the machines Powder runs on: 64-bit ones on
-- which a long is 64 bits.
module Foreign.Base where

import Data.Int
import Data.Word
import Foreign.C.Types
import Powder.Base (IOError (..), IOErrorType (..), Ref (..), atExit, catchIOError, newRef, readRef, writeRef)

-- Addresses -----------------------------------------------------------------

-- An address, of something of a type. It is a number.
newtype Ptr a = Ptr Integer

instance Eq (Ptr a) where
  Ptr a == Ptr b = a == b

instance Ord (Ptr a) where
  compare (Ptr a) (Ptr b) = compare a b

instance Show (Ptr a) where
  showsPrec _ (Ptr a) = showString "0x" . showString (replicate (16 - length digits) '0' ++ digits)
    where
      digits = hex a
      hex n = if n < 16 then [digit n] else hex (n `quot` 16) ++ [digit (n `rem` 16)]
      digit d = "0123456789abcdef" !! fromInteger d

nullPtr :: Ptr a
nullPtr = Ptr 0

castPtr :: Ptr a -> Ptr b
castPtr (Ptr a) = Ptr a

plusPtr :: Ptr a -> Int -> Ptr b
plusPtr (Ptr a) n = Ptr (a + toInteger n)

-- The next address at or after one that is a multiple of a number.
alignPtr :: Ptr a -> Int -> Ptr a
alignPtr (Ptr a) n = case a `rem` toInteger n of
  0 -> Ptr a
  r -> Ptr (a + (toInteger n - r))

minusPtr :: Ptr a -> Ptr b -> Int
minusPtr (Ptr a) (Ptr b) = fromInteger (a - b)

-- The address of a C function, of a type.
newtype FunPtr a = FunPtr Integer

instance Eq (FunPtr a) where
  FunPtr a == FunPtr b = a == b

instance Ord (FunPtr a) where
  compare (FunPtr a) (FunPtr b) = compare a b

instance Show (FunPtr a) where
  showsPrec p (FunPtr a) = showsPrec p (Ptr a)

nullFunPtr :: FunPtr a
nullFunPtr = FunPtr 0

castFunPtr :: FunPtr a -> FunPtr b
castFunPtr (FunPtr a) = FunPtr a

castFunPtrToPtr :: FunPtr a -> Ptr b
castFunPtrToPtr (FunPtr a) = Ptr a

castPtrToFunPtr :: Ptr a -> FunPtr b
castPtrToFunPtr (Ptr a) = FunPtr a

-- A C function made from one of the program's is let go of. None is made
-- yet, so there is nothing to do.
freeHaskellFunPtr :: FunPtr a -> IO ()
freeHaskellFunPtr _ = return ()

-- An address as an integer of its size, signed and not.
newtype IntPtr = IntPtr Int64

newtype WordPtr = WordPtr Word64

ptrToIntPtr :: Ptr a -> IntPtr
ptrToIntPtr (Ptr a) = IntPtr (fromInteger a)

intPtrToPtr :: IntPtr -> Ptr a
intPtrToPtr (IntPtr a) = Ptr (toInteger (fromIntegral a :: Word64))

ptrToWordPtr :: Ptr a -> WordPtr
ptrToWordPtr (Ptr a) = WordPtr (fromInteger a)

wordPtrToPtr :: WordPtr -> Ptr a
wordPtrToPtr (WordPtr a) = Ptr (toInteger a)

instance Eq IntPtr where
  IntPtr a == IntPtr b = a == b

instance Ord IntPtr where
  compare (IntPtr a) (IntPtr b) = compare a b

instance Show IntPtr where
  showsPrec p (IntPtr a) = showsPrec p a

instance Read IntPtr where
  readsPrec p s = [(IntPtr a, t) | (a, t) <- readsPrec p s]

instance Num IntPtr where
  IntPtr a + IntPtr b = IntPtr (a + b)
  IntPtr a - IntPtr b = IntPtr (a - b)
  IntPtr a * IntPtr b = IntPtr (a * b)
  negate (IntPtr a) = IntPtr (negate a)
  abs (IntPtr a) = IntPtr (abs a)
  signum (IntPtr a) = IntPtr (signum a)
  fromInteger n = IntPtr (fromInteger n)

instance Real IntPtr where
  toRational (IntPtr a) = toRational a

instance Enum IntPtr where
  toEnum n = IntPtr (toEnum n)
  fromEnum (IntPtr a) = fromEnum a

instance Integral IntPtr where
  quotRem (IntPtr a) (IntPtr b) = let (q, r) = quotRem a b in (IntPtr q, IntPtr r)
  toInteger (IntPtr a) = toInteger a

instance Bounded IntPtr where
  minBound = IntPtr minBound
  maxBound = IntPtr maxBound

instance Eq WordPtr where
  WordPtr a == WordPtr b = a == b

instance Ord WordPtr where
  compare (WordPtr a) (WordPtr b) = compare a b

instance Show WordPtr where
  showsPrec p (WordPtr a) = showsPrec p a

instance Read WordPtr where
  readsPrec p s = [(WordPtr a, t) | (a, t) <- readsPrec p s]

instance Num WordPtr where
  WordPtr a + WordPtr b = WordPtr (a + b)
  WordPtr a - WordPtr b = WordPtr (a - b)
  WordPtr a * WordPtr b = WordPtr (a * b)
  negate (WordPtr a) = WordPtr (negate a)
  abs (WordPtr a) = WordPtr (abs a)
  signum (WordPtr a) = WordPtr (signum a)
  fromInteger n = WordPtr (fromInteger n)

instance Real WordPtr where
  toRational (WordPtr a) = toRational a

instance Enum WordPtr where
  toEnum n = WordPtr (toEnum n)
  fromEnum (WordPtr a) = fromEnum a

instance Integral WordPtr where
  quotRem (WordPtr a) (WordPtr b) = let (q, r) = quotRem a b in (WordPtr q, WordPtr r)
  toInteger (WordPtr a) = toInteger a

instance Bounded WordPtr where
  minBound = WordPtr minBound
  maxBound = WordPtr maxBound

-- What can be kept ------------------------------------------------------------

-- A type whose values have a size and can be kept at an address.
class Storable a where
  sizeOf :: a -> Int
  alignment :: a -> Int
  peekElemOff :: Ptr a -> Int -> IO a
  pokeElemOff :: Ptr a -> Int -> a -> IO ()
  peekByteOff :: Ptr b -> Int -> IO a
  pokeByteOff :: Ptr b -> Int -> a -> IO ()
  peek :: Ptr a -> IO a
  poke :: Ptr a -> a -> IO ()

  peekElemOff p i = peek (plusPtr p (i * sizeOf (pointee p)))
  pokeElemOff p i x = poke (plusPtr p (i * sizeOf x)) x
  peekByteOff p i = peek (plusPtr p i)
  pokeByteOff p i = poke (plusPtr p i)
  peek p = peekElemOff p 0
  poke p = pokeElemOff p 0

-- What an address is of, which is never looked at: only its type is.
pointee :: Ptr a -> a
pointee _ = error "Foreign: a value that is only there for its type was looked at"

instance Storable Int where
  sizeOf _ = 8
  alignment _ = 8
  peek (Ptr a) = primPeekInt 3 a >>= \n -> return (fromInteger n)
  poke (Ptr a) x = primPokeInt 3 a (toInteger x)

instance Storable Word where
  sizeOf _ = 8
  alignment _ = 8
  peek (Ptr a) = primPeekInt 7 a >>= \n -> return (fromInteger n)
  poke (Ptr a) x = primPokeInt 7 a (toInteger x)

instance Storable Int8 where
  sizeOf _ = 1
  alignment _ = 1
  peek (Ptr a) = primPeekInt 0 a >>= \n -> return (fromInteger n)
  poke (Ptr a) x = primPokeInt 0 a (toInteger x)

instance Storable Int16 where
  sizeOf _ = 2
  alignment _ = 2
  peek (Ptr a) = primPeekInt 1 a >>= \n -> return (fromInteger n)
  poke (Ptr a) x = primPokeInt 1 a (toInteger x)

instance Storable Int32 where
  sizeOf _ = 4
  alignment _ = 4
  peek (Ptr a) = primPeekInt 2 a >>= \n -> return (fromInteger n)
  poke (Ptr a) x = primPokeInt 2 a (toInteger x)

instance Storable Int64 where
  sizeOf _ = 8
  alignment _ = 8
  peek (Ptr a) = primPeekInt 3 a >>= \n -> return (fromInteger n)
  poke (Ptr a) x = primPokeInt 3 a (toInteger x)

instance Storable Word8 where
  sizeOf _ = 1
  alignment _ = 1
  peek (Ptr a) = primPeekInt 4 a >>= \n -> return (fromInteger n)
  poke (Ptr a) x = primPokeInt 4 a (toInteger x)

instance Storable Word16 where
  sizeOf _ = 2
  alignment _ = 2
  peek (Ptr a) = primPeekInt 5 a >>= \n -> return (fromInteger n)
  poke (Ptr a) x = primPokeInt 5 a (toInteger x)

instance Storable Word32 where
  sizeOf _ = 4
  alignment _ = 4
  peek (Ptr a) = primPeekInt 6 a >>= \n -> return (fromInteger n)
  poke (Ptr a) x = primPokeInt 6 a (toInteger x)

instance Storable Word64 where
  sizeOf _ = 8
  alignment _ = 8
  peek (Ptr a) = primPeekInt 7 a >>= \n -> return (fromInteger n)
  poke (Ptr a) x = primPokeInt 7 a (toInteger x)

instance Storable CChar where
  sizeOf _ = 1
  alignment _ = 1
  peek (Ptr a) = primPeekInt 0 a >>= \n -> return (fromInteger n)
  poke (Ptr a) x = primPokeInt 0 a (toInteger x)

instance Storable CSChar where
  sizeOf _ = 1
  alignment _ = 1
  peek (Ptr a) = primPeekInt 0 a >>= \n -> return (fromInteger n)
  poke (Ptr a) x = primPokeInt 0 a (toInteger x)

instance Storable CUChar where
  sizeOf _ = 1
  alignment _ = 1
  peek (Ptr a) = primPeekInt 4 a >>= \n -> return (fromInteger n)
  poke (Ptr a) x = primPokeInt 4 a (toInteger x)

instance Storable CShort where
  sizeOf _ = 2
  alignment _ = 2
  peek (Ptr a) = primPeekInt 1 a >>= \n -> return (fromInteger n)
  poke (Ptr a) x = primPokeInt 1 a (toInteger x)

instance Storable CUShort where
  sizeOf _ = 2
  alignment _ = 2
  peek (Ptr a) = primPeekInt 5 a >>= \n -> return (fromInteger n)
  poke (Ptr a) x = primPokeInt 5 a (toInteger x)

instance Storable CInt where
  sizeOf _ = 4
  alignment _ = 4
  peek (Ptr a) = primPeekInt 2 a >>= \n -> return (fromInteger n)
  poke (Ptr a) x = primPokeInt 2 a (toInteger x)

instance Storable CUInt where
  sizeOf _ = 4
  alignment _ = 4
  peek (Ptr a) = primPeekInt 6 a >>= \n -> return (fromInteger n)
  poke (Ptr a) x = primPokeInt 6 a (toInteger x)

instance Storable CLong where
  sizeOf _ = 8
  alignment _ = 8
  peek (Ptr a) = primPeekInt 3 a >>= \n -> return (fromInteger n)
  poke (Ptr a) x = primPokeInt 3 a (toInteger x)

instance Storable CULong where
  sizeOf _ = 8
  alignment _ = 8
  peek (Ptr a) = primPeekInt 7 a >>= \n -> return (fromInteger n)
  poke (Ptr a) x = primPokeInt 7 a (toInteger x)

instance Storable CLLong where
  sizeOf _ = 8
  alignment _ = 8
  peek (Ptr a) = primPeekInt 3 a >>= \n -> return (fromInteger n)
  poke (Ptr a) x = primPokeInt 3 a (toInteger x)

instance Storable CULLong where
  sizeOf _ = 8
  alignment _ = 8
  peek (Ptr a) = primPeekInt 7 a >>= \n -> return (fromInteger n)
  poke (Ptr a) x = primPokeInt 7 a (toInteger x)

instance Storable CSize where
  sizeOf _ = 8
  alignment _ = 8
  peek (Ptr a) = primPeekInt 7 a >>= \n -> return (fromInteger n)
  poke (Ptr a) x = primPokeInt 7 a (toInteger x)

instance Storable CPtrdiff where
  sizeOf _ = 8
  alignment _ = 8
  peek (Ptr a) = primPeekInt 3 a >>= \n -> return (fromInteger n)
  poke (Ptr a) x = primPokeInt 3 a (toInteger x)

instance Storable CWchar where
  sizeOf _ = 4
  alignment _ = 4
  peek (Ptr a) = primPeekInt 2 a >>= \n -> return (fromInteger n)
  poke (Ptr a) x = primPokeInt 2 a (toInteger x)

instance Storable CSigAtomic where
  sizeOf _ = 4
  alignment _ = 4
  peek (Ptr a) = primPeekInt 2 a >>= \n -> return (fromInteger n)
  poke (Ptr a) x = primPokeInt 2 a (toInteger x)

instance Storable CClock where
  sizeOf _ = 8
  alignment _ = 8
  peek (Ptr a) = primPeekInt 3 a >>= \n -> return (fromInteger n)
  poke (Ptr a) x = primPokeInt 3 a (toInteger x)

instance Storable CTime where
  sizeOf _ = 8
  alignment _ = 8
  peek (Ptr a) = primPeekInt 3 a >>= \n -> return (fromInteger n)
  poke (Ptr a) x = primPokeInt 3 a (toInteger x)

instance Storable CIntPtr where
  sizeOf _ = 8
  alignment _ = 8
  peek (Ptr a) = primPeekInt 3 a >>= \n -> return (fromInteger n)
  poke (Ptr a) x = primPokeInt 3 a (toInteger x)

instance Storable CUIntPtr where
  sizeOf _ = 8
  alignment _ = 8
  peek (Ptr a) = primPeekInt 7 a >>= \n -> return (fromInteger n)
  poke (Ptr a) x = primPokeInt 7 a (toInteger x)

instance Storable CIntMax where
  sizeOf _ = 8
  alignment _ = 8
  peek (Ptr a) = primPeekInt 3 a >>= \n -> return (fromInteger n)
  poke (Ptr a) x = primPokeInt 3 a (toInteger x)

instance Storable CUIntMax where
  sizeOf _ = 8
  alignment _ = 8
  peek (Ptr a) = primPeekInt 7 a >>= \n -> return (fromInteger n)
  poke (Ptr a) x = primPokeInt 7 a (toInteger x)

instance Storable Double where
  sizeOf _ = 8
  alignment _ = 8
  peek (Ptr a) = primPeekDouble 9 a >>= \x -> return x
  poke (Ptr a) x = primPokeDouble 9 a x

instance Storable Float where
  sizeOf _ = 4
  alignment _ = 4
  peek (Ptr a) = primPeekDouble 8 a >>= \x -> return (realToFrac x)
  poke (Ptr a) x = primPokeDouble 8 a (realToFrac x)

instance Storable CDouble where
  sizeOf _ = 8
  alignment _ = 8
  peek (Ptr a) = primPeekDouble 9 a >>= \x -> return (realToFrac x)
  poke (Ptr a) x = primPokeDouble 9 a (realToFrac x)

instance Storable CFloat where
  sizeOf _ = 4
  alignment _ = 4
  peek (Ptr a) = primPeekDouble 8 a >>= \x -> return (realToFrac x)
  poke (Ptr a) x = primPokeDouble 8 a (realToFrac x)

-- A character is its code, in 32 bits; and a Bool is a C int.
instance Storable Char where
  sizeOf _ = 4
  alignment _ = 4
  peek (Ptr a) = primPeekInt 6 a >>= \n -> return (toEnum (fromInteger n))
  poke (Ptr a) c = primPokeInt 6 a (toInteger (fromEnum c))

instance Storable Bool where
  sizeOf _ = 4
  alignment _ = 4
  peek (Ptr a) = primPeekInt 2 a >>= \n -> return (n /= 0)
  poke (Ptr a) b = primPokeInt 2 a (if b then 1 else 0)

instance Storable (Ptr a) where
  sizeOf _ = 8
  alignment _ = 8
  peek (Ptr a) = primPeekInt 7 a >>= \n -> return (Ptr n)
  poke (Ptr a) (Ptr x) = primPokeInt 7 a x

instance Storable (FunPtr a) where
  sizeOf _ = 8
  alignment _ = 8
  peek (Ptr a) = primPeekInt 7 a >>= \n -> return (FunPtr n)
  poke (Ptr a) (FunPtr x) = primPokeInt 7 a x

instance Storable (StablePtr a) where
  sizeOf _ = 8
  alignment _ = 8
  peek (Ptr a) = primPeekInt 7 a >>= \n -> return (StablePtr n)
  poke (Ptr a) (StablePtr x) = primPokeInt 7 a x

instance Storable IntPtr where
  sizeOf _ = 8
  alignment _ = 8
  peek (Ptr a) = primPeekInt 3 a >>= \n -> return (fromInteger n)
  poke (Ptr a) x = primPokeInt 3 a (toInteger x)

instance Storable WordPtr where
  sizeOf _ = 8
  alignment _ = 8
  peek (Ptr a) = primPeekInt 7 a >>= \n -> return (fromInteger n)
  poke (Ptr a) x = primPokeInt 7 a (toInteger x)

-- Values C can hold on to ---------------------------------------------------

-- Something of the program's that C can be given to keep and hand back:
-- to C it is an address, though nothing is at it. It is kept until it is
-- freed.
newtype StablePtr a = StablePtr Integer

instance Eq (StablePtr a) where
  StablePtr a == StablePtr b = a == b

-- The number is one more than the cell's, so that none is null.
newStablePtr :: a -> IO (StablePtr a)
newStablePtr x = newRef (Just x) >>= \(Ref n) -> return (StablePtr (n + 1))

deRefStablePtr :: StablePtr a -> IO a
deRefStablePtr (StablePtr n) =
  readRef (Ref (n - 1)) >>= \kept -> case kept of
    Just x -> return x
    Nothing -> ioError (IOError IllegalOperation "deRefStablePtr" "the stable pointer has been freed" Nothing Nothing)

freeStablePtr :: StablePtr a -> IO ()
freeStablePtr (StablePtr n) = writeRef (Ref (n - 1)) Nothing

castStablePtrToPtr :: StablePtr a -> Ptr ()
castStablePtrToPtr (StablePtr n) = Ptr n

castPtrToStablePtr :: Ptr () -> StablePtr a
castPtrToStablePtr (Ptr n) = StablePtr n

-- Memory --------------------------------------------------------------------

foreign import ccall "stdlib.h malloc" c_malloc :: CSize -> IO (Ptr a)

foreign import ccall "stdlib.h realloc" c_realloc :: Ptr a -> CSize -> IO (Ptr b)

foreign import ccall "stdlib.h free" free :: Ptr a -> IO ()

foreign import ccall "stdlib.h &free" finalizerFree :: FunPtr (Ptr a -> IO ())

foreign import ccall "string.h memcpy" c_memcpy :: Ptr a -> Ptr b -> CSize -> IO (Ptr a)

foreign import ccall "string.h memmove" c_memmove :: Ptr a -> Ptr b -> CSize -> IO (Ptr a)

-- An address that is null is an error: there was no memory to be had.
given :: String -> Ptr a -> IO (Ptr a)
given place p = if p == nullPtr then ioError (IOError ResourceExhausted place "out of memory" Nothing Nothing) else return p

mallocBytes :: Int -> IO (Ptr a)
mallocBytes n = c_malloc (fromIntegral n) >>= given "malloc"

malloc :: Storable a => IO (Ptr a)
malloc = sized undefined
  where
    sized :: Storable b => b -> IO (Ptr b)
    sized x = mallocBytes (sizeOf x)

reallocBytes :: Ptr a -> Int -> IO (Ptr a)
reallocBytes p n = if n == 0 then free p >> return nullPtr else c_realloc p (fromIntegral n) >>= given "realloc"

realloc :: Storable b => Ptr a -> IO (Ptr b)
realloc p = sized undefined
  where
    sized :: Storable c => c -> IO (Ptr c)
    sized x = c_realloc p (fromIntegral (sizeOf x)) >>= given "realloc"

-- Memory for as long as an action runs, however it ends.
allocaBytes :: Int -> (Ptr a -> IO b) -> IO b
allocaBytes n act = do
  p <- mallocBytes n
  x <- catchIOError (act p) (\e -> free p >> ioError e)
  free p
  return x

alloca :: Storable a => (Ptr a -> IO b) -> IO b
alloca = sized undefined
  where
    sized :: Storable c => c -> (Ptr c -> IO d) -> IO d
    sized x = allocaBytes (sizeOf x)

copyBytes :: Ptr a -> Ptr a -> Int -> IO ()
copyBytes to from n = c_memcpy to from (fromIntegral n) >> return ()

moveBytes :: Ptr a -> Ptr a -> Int -> IO ()
moveBytes to from n = c_memmove to from (fromIntegral n) >> return ()

-- Addresses with something to do at the end ----------------------------------

-- A function of C's to call on an address when it is done with.
type FinalizerPtr a = FunPtr (Ptr a -> IO ())

foreign import ccall "dynamic" finalizing :: FunPtr (Ptr a -> IO ()) -> Ptr a -> IO ()

-- An address, with what is to be done when the program is done with it.
-- Nothing here finds out when the last use of one has been made, so that
-- is when it is asked for, with finalizeForeignPtr, or when the program
-- ends, whichever is first.
data ForeignPtr a = ForeignPtr (Ptr a) (Ref [IO ()])

instance Eq (ForeignPtr a) where
  ForeignPtr a _ == ForeignPtr b _ = a == b

instance Ord (ForeignPtr a) where
  compare (ForeignPtr a _) (ForeignPtr b _) = compare a b

instance Show (ForeignPtr a) where
  showsPrec p (ForeignPtr a _) = showsPrec p a

newForeignPtr_ :: Ptr a -> IO (ForeignPtr a)
newForeignPtr_ p = do
  r <- newRef []
  let fp = ForeignPtr p r
  atExit (finalizeForeignPtr fp)
  return fp

newForeignPtr :: FinalizerPtr a -> Ptr a -> IO (ForeignPtr a)
newForeignPtr finalizer p = newForeignPtr_ p >>= \fp -> addForeignPtrFinalizer finalizer fp >> return fp

-- The last added is the first run.
addForeignPtrFinalizer :: FinalizerPtr a -> ForeignPtr a -> IO ()
addForeignPtrFinalizer finalizer (ForeignPtr p r) = readRef r >>= \fs -> writeRef r (finalizing finalizer p : fs)

finalizeForeignPtr :: ForeignPtr a -> IO ()
finalizeForeignPtr (ForeignPtr _ r) = readRef r >>= \fs -> writeRef r [] >> sequence_ fs

withForeignPtr :: ForeignPtr a -> (Ptr a -> IO b) -> IO b
withForeignPtr (ForeignPtr p _) act = act p

unsafeForeignPtrToPtr :: ForeignPtr a -> Ptr a
unsafeForeignPtrToPtr (ForeignPtr p _) = p

touchForeignPtr :: ForeignPtr a -> IO ()
touchForeignPtr _ = return ()

castForeignPtr :: ForeignPtr a -> ForeignPtr b
castForeignPtr (ForeignPtr p r) = ForeignPtr (castPtr p) r

mallocForeignPtrBytes :: Int -> IO (ForeignPtr a)
mallocForeignPtrBytes n = mallocBytes n >>= newForeignPtr finalizerFree

mallocForeignPtr :: Storable a => IO (ForeignPtr a)
mallocForeignPtr = malloc >>= newForeignPtr finalizerFree

mallocForeignPtrArray :: Storable a => Int -> IO (ForeignPtr a)
mallocForeignPtrArray n = mallocArray n >>= newForeignPtr finalizerFree

mallocForeignPtrArray0 :: Storable a => Int -> IO (ForeignPtr a)
mallocForeignPtrArray0 n = mallocArray0 n >>= newForeignPtr finalizerFree

-- Arrays --------------------------------------------------------------------

mallocArray :: Storable a => Int -> IO (Ptr a)
mallocArray = sized undefined
  where
    sized :: Storable b => b -> Int -> IO (Ptr b)
    sized x n = mallocBytes (n * sizeOf x)

mallocArray0 :: Storable a => Int -> IO (Ptr a)
mallocArray0 n = mallocArray (n + 1)

allocaArray :: Storable a => Int -> (Ptr a -> IO b) -> IO b
allocaArray = sized undefined
  where
    sized :: Storable c => c -> Int -> (Ptr c -> IO d) -> IO d
    sized x n = allocaBytes (n * sizeOf x)

allocaArray0 :: Storable a => Int -> (Ptr a -> IO b) -> IO b
allocaArray0 n = allocaArray (n + 1)

reallocArray :: Storable a => Ptr a -> Int -> IO (Ptr a)
reallocArray p n = reallocBytes p (n * sizeOf (pointee p))

reallocArray0 :: Storable a => Ptr a -> Int -> IO (Ptr a)
reallocArray0 p n = reallocArray p (n + 1)

peekArray :: Storable a => Int -> Ptr a -> IO [a]
peekArray n p = mapM (peekElemOff p) (enumFromTo 0 (n - 1))

-- What is at an address up to, and not counting, the first that is an end.
peekArray0 :: (Storable a, Eq a) => a -> Ptr a -> IO [a]
peekArray0 end p = lengthArray0 end p >>= \n -> peekArray n p

pokeArray :: Storable a => Ptr a -> [a] -> IO ()
pokeArray p xs = sequence_ (zipWith (pokeElemOff p) (enumFrom 0) xs)

pokeArray0 :: Storable a => a -> Ptr a -> [a] -> IO ()
pokeArray0 end p xs = pokeArray p xs >> pokeElemOff p (length xs) end

newArray :: Storable a => [a] -> IO (Ptr a)
newArray xs = mallocArray (length xs) >>= \p -> pokeArray p xs >> return p

newArray0 :: Storable a => a -> [a] -> IO (Ptr a)
newArray0 end xs = mallocArray0 (length xs) >>= \p -> pokeArray0 end p xs >> return p

withArray :: Storable a => [a] -> (Ptr a -> IO b) -> IO b
withArray xs act = withArrayLen xs (\_ p -> act p)

withArray0 :: Storable a => a -> [a] -> (Ptr a -> IO b) -> IO b
withArray0 end xs act = withArrayLen0 end xs (\_ p -> act p)

withArrayLen :: Storable a => [a] -> (Int -> Ptr a -> IO b) -> IO b
withArrayLen xs act = allocaArray (length xs) (\p -> pokeArray p xs >> act (length xs) p)

withArrayLen0 :: Storable a => a -> [a] -> (Int -> Ptr a -> IO b) -> IO b
withArrayLen0 end xs act = allocaArray0 (length xs) (\p -> pokeArray0 end p xs >> act (length xs) p)

copyArray :: Storable a => Ptr a -> Ptr a -> Int -> IO ()
copyArray to from n = copyBytes to from (n * sizeOf (pointee to))

moveArray :: Storable a => Ptr a -> Ptr a -> Int -> IO ()
moveArray to from n = moveBytes to from (n * sizeOf (pointee to))

lengthArray0 :: (Storable a, Eq a) => a -> Ptr a -> IO Int
lengthArray0 end p = go 0
  where
    go i = peekElemOff p i >>= \x -> if x == end then return i else go (i + 1)

advancePtr :: Storable a => Ptr a -> Int -> Ptr a
advancePtr p n = plusPtr p (n * sizeOf (pointee p))

-- One value at a time -------------------------------------------------------

with :: Storable a => a -> (Ptr a -> IO b) -> IO b
with x act = alloca (\p -> poke p x >> act p)

new :: Storable a => a -> IO (Ptr a)
new x = malloc >>= \p -> poke p x >> return p

fromBool :: Num a => Bool -> a
fromBool b = if b then 1 else 0

toBool :: (Eq a, Num a) => a -> Bool
toBool = (/= 0)

maybeNew :: (a -> IO (Ptr b)) -> Maybe a -> IO (Ptr b)
maybeNew = maybe (return nullPtr)

maybeWith :: (a -> (Ptr b -> IO c) -> IO c) -> Maybe a -> (Ptr b -> IO c) -> IO c
maybeWith = maybe (\act -> act nullPtr)

maybePeek :: (Ptr a -> IO b) -> Ptr a -> IO (Maybe b)
maybePeek get p = if p == nullPtr then return Nothing else get p >>= return . Just

withMany :: (a -> (b -> res) -> res) -> [a] -> ([b] -> res) -> res
withMany _ [] f = f []
withMany each (x : xs) f = each x (\y -> withMany each xs (\ys -> f (y : ys)))

-- What went wrong -------------------------------------------------------------

throwIf :: (a -> Bool) -> (a -> String) -> IO a -> IO a
throwIf bad says act = act >>= \x -> if bad x then ioError (userError (says x)) else return x

throwIf_ :: (a -> Bool) -> (a -> String) -> IO a -> IO ()
throwIf_ bad says act = throwIf bad says act >> return ()

throwIfNeg :: (Ord a, Num a) => (a -> String) -> IO a -> IO a
throwIfNeg = throwIf (< 0)

throwIfNeg_ :: (Ord a, Num a) => (a -> String) -> IO a -> IO ()
throwIfNeg_ = throwIf_ (< 0)

throwIfNull :: String -> IO (Ptr a) -> IO (Ptr a)
throwIfNull message = throwIf (== nullPtr) (const message)

void :: IO a -> IO ()
void act = act >> return ()
