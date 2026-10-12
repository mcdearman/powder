-- The prelude of a module written in the Haskell2010 edition: the names
-- the Report says the Prelude exports, and no others, so that a program of
-- the Report's may use any other name for something of its own. They are
-- the same things the prelude of every other edition has, and so the
-- classes are today's and not the Report's: Monad has Applicative above
-- it, and fail is MonadFail's.
module Powder.Prelude2010
  ( Maybe (..), Either (..), Ordering (..), String, Rational,
    Eq (..), Ord (..), Enum (..), Bounded (..),
    Num (..), Real (..), Integral (..), Fractional (..), Floating (..), RealFrac (..), RealFloat (..),
    Functor (fmap), Monad (..), fail, mapM, mapM_, sequence, sequence_, (=<<),
    maybe, either, (&&), (||), not, otherwise,
    subtract, even, odd, gcd, lcm, (^), (^^), fromIntegral, realToFrac,
    fst, snd, curry, uncurry, id, const, (.), flip, ($), until, asTypeOf, error, undefined, seq, ($!),
    map, (++), filter, head, last, tail, init, null, length, (!!), reverse,
    foldl, foldl1, foldr, foldr1, and, or, any, all, sum, product, concat, concatMap, maximum, minimum,
    scanl, scanl1, scanr, scanr1, iterate, repeat, replicate, cycle,
    take, drop, splitAt, takeWhile, dropWhile, span, break,
    elem, notElem, lookup, zip, zip3, zipWith, zipWith3, unzip, unzip3,
    lines, words, unlines, unwords,
    ShowS, Show (..), shows, showChar, showString, showParen,
    ReadS, Read (..), reads, readParen, read, lex,
    putChar, putStr, putStrLn, print, getChar, getLine, getContents, interact,
    FilePath, readFile, writeFile, appendFile, readIO, readLn, IOError, ioError, userError
  )
where

import Powder.Base
