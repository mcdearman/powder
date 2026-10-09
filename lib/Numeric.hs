module Numeric
  ( showSigned, showIntAtBase, showInt, showHex, showOct, showEFloat, showFFloat, showGFloat, showFloat, floatToDigits,
    readSigned, readInt, readDec, readOct, readHex, readFloat, lexDigits, fromRat
  )
where

import Data.Char (intToDigit)
import Powder.Base (lexDigits, readDec, readFloat, readHex, readInt, readOct, readSigned)

showSigned :: Real a => (a -> ShowS) -> Int -> a -> ShowS
showSigned showPos p x
  | x < 0 = showParen (p > 6) (showChar '-' . showPos (negate x))
  | otherwise = showPos x

showIntAtBase :: Integral a => a -> (Int -> Char) -> a -> ShowS
showIntAtBase base digit n rest
  | n < 0 = error "Numeric.showIntAtBase: negative number"
  | n < base = digit (fromIntegral n) : rest
  | otherwise = showIntAtBase base digit (quot n base) (digit (fromIntegral (rem n base)) : rest)

showInt, showHex, showOct :: Integral a => a -> ShowS
showInt = showIntAtBase 10 intToDigit
showHex = showIntAtBase 16 intToDigit
showOct = showIntAtBase 8 intToDigit

fromRat :: RealFloat a => Rational -> a
fromRat = fromRational

-- With an exponent, plainly, or whichever suits the size of the number;
-- and to a number of digits after the point, or as many as it has.
showEFloat, showFFloat, showGFloat :: RealFloat a => Maybe Int -> a -> ShowS
showEFloat d x = showString (formatted Exponent d x)
showFFloat d x = showString (formatted Fixed d x)
showGFloat d x = showString (formatted Generic d x)

showFloat :: RealFloat a => a -> ShowS
showFloat x = showString (formatted Generic Nothing x)

data Format = Exponent | Fixed | Generic

formatted :: RealFloat a => Format -> Maybe Int -> a -> String
formatted fmt decs x
  | isNaN x = "NaN"
  | isInfinite x = if x < 0 then "-Infinity" else "Infinity"
  | x < 0 || isNegativeZero x = '-' : shaped fmt (floatToDigits 10 (negate x))
  | otherwise = shaped fmt (floatToDigits 10 x)
  where
    shaped format (is, e) = case format of
      Generic -> shaped (if e < 0 || e > 7 then Exponent else Fixed) (is, e)
      Exponent -> case decs of
        Nothing -> case map intToDigit is of
          "0" -> "0.0e0"
          [d] -> d : ".0e" ++ show (e - 1)
          d : ds -> d : '.' : ds ++ "e" ++ show (e - 1)
          [] -> error "Numeric: a number with no digits"
        Just dec ->
          let dec' = max dec 1
           in case is of
                [0] -> '0' : '.' : replicate dec' '0' ++ "e0"
                _ ->
                  let (ei, is') = roundTo 10 (dec' + 1) is
                   in case map intToDigit (if ei > 0 then init is' else is') of
                        d : ds -> d : '.' : ds ++ 'e' : show (e - 1 + ei)
                        [] -> error "Numeric: a number with no digits"
      Fixed -> case decs of
        Nothing
          | e <= 0 -> "0." ++ replicate (negate e) '0' ++ map intToDigit is
          | otherwise -> pointed e "" (map intToDigit is)
        Just dec ->
          let dec' = max dec 0
           in if e >= 0
                then
                  let (ei, is') = roundTo 10 (dec' + e) is
                      (ls, rs) = splitAt (e + ei) (map intToDigit is')
                   in orZero ls ++ (if null rs then "" else '.' : rs)
                else
                  let (ei, is') = roundTo 10 dec' (replicate (negate e) 0 ++ is)
                   in case map intToDigit (if ei > 0 then is' else 0 : is') of
                        d : ds -> d : (if null ds then "" else '.' : ds)
                        [] -> error "Numeric: a number with no digits"
    orZero ls = if null ls then "0" else ls
    -- Put the point after so many digits, with zeros where there are none.
    pointed :: Int -> String -> String -> String
    pointed 0 s rs = orZero (reverse s) ++ '.' : orZero rs
    pointed n s "" = pointed (n - 1) ('0' : s) ""
    pointed n s (r : rs) = pointed (n - 1) (r : s) rs

-- Round digits to so many, giving whether that carried into a new first
-- digit. Half goes to the even digit.
roundTo :: Int -> Int -> [Int] -> (Int, [Int])
roundTo base d is = case go d True is of
  (0, xs) -> (0, xs)
  (_, xs) -> (1, 1 : xs)
  where
    half = quot base 2
    go n _ [] = (0, replicate n 0)
    go 0 e (x : xs)
      | x == half && e && all (== 0) xs = (0, [])
      | otherwise = (if x >= half then 1 else 0, [])
    go n _ (i : xs) =
      let (c, rest) = go (n - 1) (even i) xs
          i' = c + i
       in if i' == base then (1, 0 : rest) else (0, i' : rest)

-- The digits of a number that is not negative, in a base, and where the
-- point goes: the fewest digits that are this number and no other of its
-- type. After Burger and Dybvig, "Printing floating-point numbers quickly
-- and accurately".
floatToDigits :: RealFloat a => Integer -> a -> ([Int], Int)
floatToDigits base x
  | x == 0 = ([0], 0)
  | otherwise = (map fromInteger (reverse digits), k)
  where
    (f0, e0) = decodeFloat x
    (least0, _) = floatRange x
    p = floatDigits x
    b = floatRadix x
    least = least0 - p
    -- A number too small to have all its digits has fewer.
    (f, e) = let n = least - e0 in if n > 0 then (quot f0 (b ^ n), e0 + n) else (f0, e0)
    (r, s, up, down)
      | e >= 0 = let be = b ^ e in if f == b ^ (p - 1) then (f * be * b * 2, 2 * b, be * b, be) else (f * be * 2, 2, be, be)
      | e > least && f == b ^ (p - 1) = (f * b * 2, b ^ (negate e + 1) * 2, b, 1)
      | otherwise = (f * 2, b ^ negate e * 2, 1, 1)
    -- Where the point goes: the least power of the base that is more than
    -- the number, found from below.
    k = fixup (estimate - 2)
    estimate :: Int
    estimate = if b == 2 && base == 10 then quot ((p - 1 + e0) * 8651) 28738 else truncate ((fromIntegral (p + e0) :: Double) * log (fromInteger b) / log (fromInteger base))
    fixup :: Int -> Int
    fixup n
      | n >= 0 = if r + up <= base ^ n * s then n else fixup (n + 1)
      | otherwise = if base ^ negate n * (r + up) <= s then n else fixup (n + 1)
    digits = if k >= 0 then generate [] r (s * base ^ k) up down else let bk = base ^ negate k in generate [] (r * bk) s (up * bk) (down * bk)
    generate ds rn sn upn downn =
      let (dn, rn') = quotRem (rn * base) sn
          upn' = upn * base
          downn' = downn * base
       in case (rn' < downn', rn' + upn' > sn) of
            (True, False) -> dn : ds
            (False, True) -> dn + 1 : ds
            (True, True) -> if rn' * 2 < sn then dn : ds else dn + 1 : ds
            (False, False) -> generate (dn : ds) rn' sn upn' downn'
