{-# LANGUAGE CPP #-}
-- CPP: the C preprocessor's directives, carried out before the file is read. Lines are kept or left out by what
-- is defined, and a name stands for text
module Main (main) where

#define ANSWER 42
#define GREETING "hello"
#define TWICE(x) ((x) + (x))
#define BETWEEN(lo, x, hi) ((lo) <= (x) && (x) <= (hi))
#define LONG_ONE 1 + \
  2 + \
  3

#ifdef ANSWER
defined' :: String
defined' = "ANSWER is defined"
#else
defined' :: String
defined' = "ANSWER is not defined"
#endif

#ifndef NOWHERE
missing :: String
missing = "NOWHERE is not defined"
#else
this is not Haskell, and is never read
#endif

#if ANSWER > 40 && ANSWER < 50
range :: String
range = "in the forties"
#elif ANSWER >= 50
range :: String
range = "fifty or more"
#else
range :: String
range = "under forty"
#endif

#if defined(ANSWER) && !defined(NOWHERE)
both :: Bool
both = True
#else
both :: Bool
both = False
#endif

#if 0
nothing here is read: not even an unclosed "string
#  if 1
nor this
#  endif
#elif (2 * 3 == 6) && (7 / 2 == 3) && (7 % 2 == 1) && (0x10 == 16) && (-1 < 0)
arithmetic :: String
arithmetic = "arithmetic"
#else
arithmetic :: String
arithmetic = "no arithmetic"
#endif

#if UNDEFINED_NAME
undefinedIsNothing :: Bool
undefinedIsNothing = False
#else
undefinedIsNothing :: Bool
undefinedIsNothing = True
#endif

#if 1
#  if 0
nested :: String
nested = "inner"
#  else
nested :: String
nested = "outer, and the inner's other branch"
#  endif
#endif

#define STAGE 1
#undef STAGE
#ifdef STAGE
stage :: String
stage = "still defined"
#else
stage :: String
stage = "undefined again"
#endif

#define STAGE 2
#if STAGE == 2
restaged :: Int
restaged = STAGE
#endif

-- A name inside a string is left alone, and one inside another name is no name.
quoted :: String
quoted = "ANSWER and TWICE(1) stay as they are"

data ANSWERS = ANSWERS Int deriving (Show)

main :: IO ()
main = do
  putStrLn defined'
  putStrLn missing
  putStrLn range
  print (both, undefinedIsNothing)
  putStrLn arithmetic
  putStrLn nested
  putStrLn stage
  print (restaged, ANSWER, TWICE(ANSWER), TWICE (3 * 2), LONG_ONE)
  putStrLn (GREETING ++ ", " ++ quoted)
  print (BETWEEN(1, ANSWER, 100), BETWEEN (50, ANSWER, 100), ANSWERS ANSWER)
  print (filter (\x -> BETWEEN(TWICE(1), x, TWICE(2))) [1 .. 6 :: Int])
