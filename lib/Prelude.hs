-- The prelude: what every module has in scope unless it says otherwise.
-- It is what the Report says the Prelude exports, taken from Powder.Base,
-- all of it. The types the language itself knows, such as Bool, Int and IO, are
-- in scope everywhere and need no exporting.
module Prelude
  ( Maybe (..), Either (..), Ordering (..), String, Rational,
    Eq (..), Ord (..), Enum (..), Bounded (..),
    Num (..), Real (..), Integral (..), Fractional (..), Floating (..), RealFrac (..), RealFloat (..),
    Functor (..), Monad (..), mapM, mapM_, sequence, sequence_, (=<<),
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
