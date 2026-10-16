{-# LANGUAGE GHC2021 #-}
-- Control.Exception: raised by actions and by values being worked out, caught by type, and a thunk that raised
-- raising again when it is forced again
module Main (main) where

import Control.Exception
import Data.IORef
import System.IO.Error (catchIOError, ioeGetErrorString, isUserError, userError)
import System.IO.Unsafe (unsafePerformIO)

data Oops = Oops Int deriving (Show)

instance Exception Oops

data Other = Other deriving (Show)

instance Exception Other

-- How many times a value's working out has begun.
counter :: IORef Int
counter = unsafePerformIO (newIORef 0)
{-# NOINLINE counter #-}

-- A value that raises the first time it is worked out, after counting.
fragile :: Int
fragile = unsafePerformIO (modifyIORef counter (+ 1) >> throwIO (Oops 1))
{-# NOINLINE fragile #-}

main :: IO ()
main = do
  -- error, caught, from inside a value worked out by print.
  r1 <- try (evaluate (length [1, 2, error "in a list"]))
  print (either (\(ErrorCall m) -> "caught: " ++ m) show r1)
  r2 <- try (evaluate (1 + error "deep" :: Int))
  print (either (\e -> "caught: " ++ show (e :: ErrorCall)) show r2)
  -- A lazy value made inside a catch and forced outside it is not caught by it.
  v <- catch (return (error "later" :: Int)) (\(ErrorCall _) -> return 0)
  r3 <- try (evaluate v)
  print (either (\(ErrorCall m) -> "outside: " ++ m) show r3)
  -- By type: the inner catch is for another type, so the outer one takes it.
  r4 <- try (catch (throwIO (Oops 7)) (\Other -> return "wrong handler"))
  print (either (\(Oops n) -> "outer got " ++ show n) id r4)
  r5 <- catch (evaluate (throw (Oops 8) :: Int) >> return "no") (\(Oops n) -> return ("pure throw " ++ show n))
  putStrLn r5
  -- SomeException takes everything.
  r6 <- catch (evaluate (head ([] :: [Int])) >> return "no") (\e -> return ("some: " ++ takeWhile (/= '\n') (show (e :: SomeException)) /= "" `seq` "caught head"))
  putStrLn r6
  -- An IOError, from an action and from a value.
  r7 <- catchIOError (ioError (userError "io one")) (\e -> return ("io: " ++ ioeGetErrorString e))
  putStrLn r7
  r8 <- try (evaluate (unsafePerformIO (ioError (userError "io two")) :: Int))
  print (either (\e -> (isUserError e, ioeGetErrorString e)) (const (False, "")) r8)
  -- A thunk that raised raises again, and is not worked out again.
  a <- try (evaluate fragile)
  b <- try (evaluate fragile)
  n <- readIORef counter
  print (either (\(Oops k) -> k) id a, either (\(Oops k) -> k) id b, n)
  -- Handlers in order; finally and bracket run their ends either way.
  r9 <- catches (evaluate (throw Other) >> return "none") [Handler (\(Oops _) -> return "oops"), Handler (\Other -> return "other")]
  putStrLn r9
  log' <- newIORef []
  let note s = modifyIORef log' (s :)
  _ <- try (bracket (note "acquire" >> return 3) (\_ -> note "release") (\x -> note ("use " ++ show x) >> throwIO (Oops x))) :: IO (Either Oops ())
  _ <- (note "body" >> return ()) `finally` note "finally"
  _ <- try ((note "raising" >> throwIO Other) `finally` note "finally again") :: IO (Either Other ())
  _ <- try (handle (\(ErrorCall m) -> note ("handled " ++ m)) (evaluate (error "x") >> return ())) :: IO (Either SomeException ())
  readIORef log' >>= print . reverse
  -- catchJust, tryJust, and what is shown.
  r10 <- catchJust (\(Oops n) -> if n > 5 then Just n else Nothing) (throwIO (Oops 9)) (return . (* 10))
  r11 <- tryJust (\(ErrorCall m) -> if m == "pick me" then Just m else Nothing) (evaluate (error "pick me"))
  print (r10, r11 :: Either String Int)
  print (show (toException (Oops 3)), displayException (ErrorCall "shown"), fmap (\(Oops n) -> n) (fromException (toException (Oops 4))), fmap (\Other -> ()) (fromException (toException (Oops 4))))
  -- Work done in a catch's handler can raise again, to the next catch out.
  r12 <- try (catch (throwIO (Oops 1)) (\(Oops n) -> throwIO (Oops (n + 1))))
  print (either (\(Oops n) -> n) id r12)
