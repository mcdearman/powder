-- Haskell 2010 Report 6.3.1 / 6.4: Eq on IEEE floating point; NaN /= NaN, 0.0 == -0.0, infinities
nan :: Double
nan = 0 / 0

inf :: Double
inf = 1 / 0

nanF :: Float
nanF = 0 / 0

negZero :: Double
negZero = -0.0

main :: IO ()
main = do
  print ("nan == nan", nan == nan)
  print ("nan /= nan", nan /= nan)
  print ("nan vs number", nan == 1, nan /= 1, nan == inf)
  print ("float nan", nanF == nanF, nanF /= nanF)
  print ("zero == negative zero", (0.0 :: Double) == negZero, (0.0 :: Double) /= negZero)
  print ("negative zero is negative zero", isNegativeZero negZero, isNegativeZero (0.0 :: Double))
  print ("float zeros", (0.0 :: Float) == (-0.0), isNegativeZero (-0.0 :: Float))
  print ("infinities", inf == inf, inf == negate inf, negate inf == negate inf, inf /= inf)
  print ("inf arithmetic", inf + 1 == inf, inf - inf == inf - inf, isNaN (inf - inf))
  print ("lists with nan", [nan] == [nan], [1, nan] /= [1, nan], (nan, 'a') == (nan, 'a'))
  print ("Maybe nan", Just nan == Just nan, Just nan /= Just nan)
  print ("elem nan", nan `elem` [nan, 1], 1 `elem` [nan, 1])
  print ("ordering with nan", nan < 1, nan > 1, nan <= nan, nan >= nan, 1 < nan)
  print ("rounding visible", (0.1 :: Double) + 0.2 == 0.3, (0.5 :: Double) + 0.25 == 0.75)
  print ("float vs double literal", realToFrac (0.5 :: Float) == (0.5 :: Double), realToFrac (0.1 :: Float) == (0.1 :: Double))
  print ("large", (1.0e308 :: Double) * 10 == inf, (2 :: Double) ^ (53 :: Int) + 1 == 2 ^ (53 :: Int))
