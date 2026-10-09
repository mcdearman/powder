-- Haskell 2010 Report 6.3.4: user Enum instances; only toEnum/fromEnum given (class defaults), and a derived instance
data Dir = N | E | S | W deriving (Eq, Show)

instance Enum Dir where
  fromEnum N = 0
  fromEnum E = 1
  fromEnum S = 2
  fromEnum W = 3
  toEnum 0 = N
  toEnum 1 = E
  toEnum 2 = S
  toEnum _ = W

data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Eq, Ord, Show, Enum, Bounded)

-- an Enum instance over a numeric wrapper, counting in steps of ten
newtype Tens = Tens Int deriving (Eq, Show)

instance Enum Tens where
  fromEnum (Tens n) = n `div` 10
  toEnum k = Tens (k * 10)

main :: IO ()
main = do
  print ("default succ/pred", succ N, succ S, pred W, pred E)
  print ("default enumFromTo", [N .. W], [E .. S], [S .. S], [W .. N])
  print ("default enumFromThenTo", [N, S .. W], [N, E .. S], [W, S .. N], [W, E .. N], [N, S .. S])
  print ("default enumFrom is lazy", take 4 [N ..], take 2 [S ..])
  print ("default enumFromThen is lazy", take 2 [N, S ..], take 4 [W, S ..])
  print ("method forms", enumFromTo N S, enumFromThenTo N E W, map fromEnum [N, E, S, W])
  print ("Tens", succ (Tens 20), pred (Tens 20), [Tens 10 .. Tens 40], [Tens 0, Tens 20 .. Tens 70])
  print ("Tens rounds through fromEnum", [Tens 15 .. Tens 39])
  print ("derived succ/pred", succ Mon, pred Sun, succ Sat)
  print ("derived codes", fromEnum Mon, fromEnum Sun, toEnum 3 :: Day)
  print ("derived enumFrom", [Fri ..], [Mon ..] == [minBound .. maxBound])
  print ("derived enumFromTo", [Tue .. Thu], [Thu .. Tue], [Sun .. Sun])
  print ("derived enumFromThen up", [Mon, Wed ..], [Tue, Fri ..])
  print ("derived enumFromThen down", [Sun, Fri ..], [Wed, Tue ..])
  print ("derived enumFromThenTo", [Mon, Wed .. Sat], [Sun, Thu .. Mon], [Mon, Tue .. Mon], [Tue, Mon .. Wed])
  print ("derived over all", map fromEnum [minBound .. maxBound :: Day], length [minBound .. maxBound :: Day])
