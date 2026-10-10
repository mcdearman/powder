-- Haskell 2010 Report 33 (Foreign.Marshal.Error): throwIf, throwIf_, throwIfNeg, throwIfNeg_, throwIfNull
-- raise an I/O error made with userError
module Main (main) where

import Foreign.Marshal.Error
import Foreign.Ptr
import System.IO.Error

attempt :: Show a => String -> IO a -> IO ()
attempt label act = tryIOError act >>= \r -> putStrLn (label ++ ": " ++ either (\e -> "raised " ++ show (isUserError e, ioeGetErrorString e)) show r)

main :: IO ()
main = do
  attempt "throwIf passes" (throwIf (> 10) (\x -> "too big: " ++ show x) (return (5 :: Int)))
  attempt "throwIf raises" (throwIf (> 10) (\x -> "too big: " ++ show x) (return (50 :: Int)))
  attempt "throwIf_ passes" (throwIf_ null (const "empty") (return "full"))
  attempt "throwIf_ raises" (throwIf_ null (const "empty") (return ""))
  attempt "throwIfNeg passes" (throwIfNeg (\x -> "negative: " ++ show x) (return (0 :: Int)))
  attempt "throwIfNeg raises" (throwIfNeg (\x -> "negative: " ++ show x) (return (-3 :: Int)))
  attempt "throwIfNeg_ raises" (throwIfNeg_ (const "negative double") (return (-0.5 :: Double)))
  attempt "throwIfNull passes" (throwIfNull "null" (return (nullPtr `plusPtr` 1 :: Ptr ())) >>= \p -> return (p /= nullPtr))
  attempt "throwIfNull raises" (throwIfNull "it was null" (return (nullPtr :: Ptr ())) >>= \p -> return (p /= nullPtr))
  attempt "the action runs once" (throwIf (const False) (const "never") (putStrLn "  effect" >> return 'x'))
