module Data.Ratio (Ratio, Rational, (%), numerator, denominator, approxRational) where

import Powder.Base (Ratio, denominator, numerator, (%))

-- The simplest ratio within a distance of a number.
approxRational :: RealFrac a => a -> a -> Rational
approxRational x eps = simplest (toRational x - toRational eps) (toRational x + toRational eps)
  where
    simplest lo hi
      | hi < lo = simplest hi lo
      | lo == hi = lo
      | lo > 0 = within (numerator lo) (denominator lo) (numerator hi) (denominator hi)
      | hi < 0 = negate (within (negate (numerator hi)) (denominator hi) (negate (numerator lo)) (denominator lo))
      | otherwise = 0
    within n d n' d'
      | r == 0 = q % 1
      | q /= q' = (q + 1) % 1
      | otherwise = let s = within d' r' d r in (q * numerator s + denominator s) % numerator s
      where
        (q, r) = quotRem n d
        (q', r') = quotRem n' d'
