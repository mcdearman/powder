module Data.Complex (Complex (..), realPart, imagPart, conjugate, mkPolar, cis, polar, magnitude, phase) where

infix 6 :+

data Complex a = !a :+ !a deriving (Eq, Read)

instance Show a => Show (Complex a) where
  showsPrec d (a :+ b) = showParen (d > 6) (showsPrec 7 a . showString " :+ " . showsPrec 7 b)

realPart, imagPart :: Complex a -> a
realPart (x :+ _) = x
imagPart (_ :+ y) = y

conjugate :: Num a => Complex a -> Complex a
conjugate (x :+ y) = x :+ negate y

mkPolar :: Floating a => a -> a -> Complex a
mkPolar r theta = (r * cos theta) :+ (r * sin theta)

cis :: Floating a => a -> Complex a
cis theta = cos theta :+ sin theta

polar :: RealFloat a => Complex a -> (a, a)
polar z = (magnitude z, phase z)

magnitude :: RealFloat a => Complex a -> a
magnitude (x :+ y) = scaleFloat k (sqrt (sqr (scaleFloat mk x) + sqr (scaleFloat mk y)))
  where
    -- Scaled so that squaring neither overflows nor loses everything.
    k = max (exponent x) (exponent y)
    mk = negate k
    sqr z = z * z

phase :: RealFloat a => Complex a -> a
phase (x :+ y) = if x == 0 && y == 0 then 0 else atan2 y x

instance RealFloat a => Num (Complex a) where
  (x :+ y) + (x' :+ y') = (x + x') :+ (y + y')
  (x :+ y) - (x' :+ y') = (x - x') :+ (y - y')
  (x :+ y) * (x' :+ y') = (x * x' - y * y') :+ (x * y' + y * x')
  negate (x :+ y) = negate x :+ negate y
  abs z = magnitude z :+ 0
  signum z@(x :+ y) = if x == 0 && y == 0 then 0 :+ 0 else let r = magnitude z in (x / r) :+ (y / r)
  fromInteger n = fromInteger n :+ 0

instance RealFloat a => Fractional (Complex a) where
  (x :+ y) / (x' :+ y') = let d = x' * x' + y' * y' in ((x * x' + y * y') / d) :+ ((y * x' - x * y') / d)
  fromRational q = fromRational q :+ 0

instance RealFloat a => Floating (Complex a) where
  pi = pi :+ 0
  exp (x :+ y) = let e = exp x in (e * cos y) :+ (e * sin y)
  log z = log (magnitude z) :+ phase z
  sqrt z@(x :+ y)
    | x == 0 && y == 0 = 0 :+ 0
    | otherwise =
        let u' = sqrt ((magnitude z + abs x) / 2)
            v' = abs y / (u' * 2)
            (u, v) = if x < 0 then (v', u') else (u', v')
         in u :+ (if y < 0 then negate v else v)
  sin (x :+ y) = (sin x * cosh y) :+ (cos x * sinh y)
  cos (x :+ y) = (cos x * cosh y) :+ negate (sin x * sinh y)
  sinh (x :+ y) = (cos y * sinh x) :+ (sin y * cosh x)
  cosh (x :+ y) = (cos y * cosh x) :+ (sin y * sinh x)
  asin z = let (x' :+ y') = log ((negate (imagPart z) :+ realPart z) + sqrt (1 - z * z)) in y' :+ negate x'
  acos z = let (x'' :+ y'') = log (z + (negate (imagPart w) :+ realPart w)); w = sqrt (1 - z * z) in y'' :+ negate x''
  atan z@(x :+ y) = let (x' :+ y') = log (((1 - y) :+ x) / sqrt (1 + z * z)) in y' :+ negate x'
  asinh z = log (z + sqrt (z * z + 1))
  acosh z = log (z + sqrt (z + 1) * sqrt (z - 1))
  atanh z = (log (1 + z) - log (1 - z)) / 2
