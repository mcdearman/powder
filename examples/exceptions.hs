-- Exceptions raised by actions and by values being worked out, caught by
-- their type; and a thunk whose working out raised, raising again.
import Control.Exception
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

data Oops = Oops Int deriving (Show)

instance Exception Oops

tries :: IORef Int
tries = unsafePerformIO (newIORef 0)

fragile :: Int
fragile = unsafePerformIO (modifyIORef tries (+ 1) >> throwIO (Oops 1))

safeDiv :: Int -> Int -> Int
safeDiv _ 0 = throw (Oops 0)
safeDiv a b = a `div` b

main :: IO ()
main = do
  r <- try (evaluate (sum (map (safeDiv 100) [5, 4, 0, 2])))
  print (either (\(Oops n) -> "divided by " ++ show n) show r)
  r' <- try (evaluate (head (drop 3 [1, 2, error "skipped", 4 :: Int])))
  print (either (\(ErrorCall m) -> m) show r')
  v <- catch (return (error "too late" :: Int)) (\(ErrorCall _) -> return 0)
  r'' <- try (evaluate v)
  print (either (\(ErrorCall m) -> "outside the catch: " ++ m) show r'')
  a <- try (evaluate fragile)
  b <- try (evaluate fragile)
  n <- readIORef tries
  print (either (\(Oops k) -> k) id a, either (\(Oops k) -> k) id b, n)
  steps <- newIORef []
  _ <- try (bracket (return 'x') (\_ -> modifyIORef steps ("release" :)) (\_ -> modifyIORef steps ("use" :) >> throwIO (Oops 2))) :: IO (Either Oops ())
  readIORef steps >>= print . reverse
  catch (ioError (userError "from IO")) (\e -> putStrLn ("caught: " ++ show (e :: IOException)))
