-- Actions run where a value is wanted, not when a program runs them: for
-- what the type system cannot see is safe.
module System.IO.Unsafe (unsafePerformIO, unsafeDupablePerformIO, unsafeInterleaveIO) where

unsafePerformIO :: IO a -> a
unsafePerformIO = primPerformIO

unsafeDupablePerformIO :: IO a -> a
unsafeDupablePerformIO = primPerformIO

-- An action whose result is worked out when it is first wanted.
unsafeInterleaveIO :: IO a -> IO a
unsafeInterleaveIO action = return (primPerformIO action)
