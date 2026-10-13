{-# LANGUAGE CPP #-}
-- An #error that is reached refuses the file
#ifndef NOWHERE
#error this is not to be compiled
#endif
main :: IO ()
main = return ()
