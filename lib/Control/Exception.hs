{-# LANGUAGE GHC2021 #-}
-- Exceptions: raised by an action, or by anything that is worked out, and
-- caught by an action that is running when they are, by their type.
module Control.Exception
  ( SomeException (..), Exception (..), ErrorCall (..), IOException, ArithException (..), ArrayException (..),
    AssertionFailed (..), throw, throwIO, catch, catches, Handler (..), handle, handleJust, catchJust, try, tryJust,
    evaluate, mapException, bracket, bracket_, bracketOnError, finally, onException, assert, mask, mask_,
    uninterruptibleMask, uninterruptibleMask_
  )
where

import Powder.Base (ErrorCall (..), Exception (..), IOError, SomeException (..), catch, throw, throwIO)

type IOException = IOError

-- Arithmetic gone wrong, which the program may raise as GHC's would.
data ArithException = Overflow | Underflow | LossOfPrecision | DivideByZero | Denormal | RatioZeroDenominator
  deriving (Eq, Ord)

instance Show ArithException where
  showsPrec _ e = showString $ case e of
    Overflow -> "arithmetic overflow"
    Underflow -> "arithmetic underflow"
    LossOfPrecision -> "loss of precision"
    DivideByZero -> "divide by zero"
    Denormal -> "denormal"
    RatioZeroDenominator -> "Ratio has zero denominator"

instance Exception ArithException

data ArrayException = IndexOutOfBounds String | UndefinedElement String
  deriving (Eq, Ord)

instance Show ArrayException where
  showsPrec _ (IndexOutOfBounds s) = showString "array index out of range" . showString (if null s then "" else ": " ++ s)
  showsPrec _ (UndefinedElement s) = showString "undefined array element" . showString (if null s then "" else ": " ++ s)

instance Exception ArrayException

newtype AssertionFailed = AssertionFailed String

instance Show AssertionFailed where
  showsPrec _ (AssertionFailed s) = showString s

instance Exception AssertionFailed

-- A handler for exceptions of one type, among several.
data Handler a = forall e. Exception e => Handler (e -> IO a)

catches :: IO a -> [Handler a] -> IO a
catches action handlers = action `catch` choose handlers
  where
    choose [] e = throwIO (e :: SomeException)
    choose (Handler h : rest) e = case fromException e of
      Just e' -> h e'
      Nothing -> choose rest e

handle :: Exception e => (e -> IO a) -> IO a -> IO a
handle = flip catch

catchJust :: Exception e => (e -> Maybe b) -> IO a -> (b -> IO a) -> IO a
catchJust p action handler = action `catch` \e -> case p e of
  Nothing -> throwIO e
  Just b -> handler b

handleJust :: Exception e => (e -> Maybe b) -> (b -> IO a) -> IO a -> IO a
handleJust p = flip (catchJust p)

try :: Exception e => IO a -> IO (Either e a)
try action = catch (fmap Right action) (return . Left)

tryJust :: Exception e => (e -> Maybe b) -> IO a -> IO (Either b a)
tryJust p action = do
  r <- try action
  case r of
    Right v -> return (Right v)
    Left e -> case p e of
      Nothing -> throwIO e
      Just b -> return (Left b)

-- A value worked out when the action runs, so that what it raises is
-- raised there.
evaluate :: a -> IO a
evaluate x = return () >>= \_ -> x `seq` return x

mapException :: (Exception e1, Exception e2) => (e1 -> e2) -> a -> a
mapException f v = v

-- Something done after an action, whether it raised or not.
finally :: IO a -> IO b -> IO a
finally action after = do
  r <- action `onException` after
  _ <- after
  return r

onException :: IO a -> IO b -> IO a
onException action what = action `catch` \e -> what >> throwIO (e :: SomeException)

bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracket acquire release use = do
  a <- acquire
  r <- use a `onException` release a
  _ <- release a
  return r

bracket_ :: IO a -> IO b -> IO c -> IO c
bracket_ before after thing = bracket before (const after) (const thing)

bracketOnError :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracketOnError acquire release use = do
  a <- acquire
  use a `onException` release a

assert :: Bool -> a -> a
assert ok x = if ok then x else throw (AssertionFailed "Assertion failed")

-- There is one thread, and nothing interrupts it: masking does nothing.
mask :: ((IO a -> IO a) -> IO b) -> IO b
mask f = f id

mask_ :: IO a -> IO a
mask_ = id

uninterruptibleMask :: ((IO a -> IO a) -> IO b) -> IO b
uninterruptibleMask f = f id

uninterruptibleMask_ :: IO a -> IO a
uninterruptibleMask_ = id
