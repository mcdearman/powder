-- Haskell 2010 Report 2.5: integer literals: decimal, octal (0o/0O), hexadecimal (0x/0X), leading zeros
module Main (main) where

main :: IO ()
main = do
  print ("decimal", 0, 7, 42, 1234567890)
  print ("leading zeros", 007, 0042, 00)
  print ("octal lower", 0o0, 0o7, 0o10, 0o777)
  print ("octal upper", 0O0, 0O17, 0O1234567)
  print ("hex lower", 0x0, 0xf, 0x10, 0xff, 0xdeadbeef)
  print ("hex upper prefix", 0X0, 0XF, 0X1F)
  print ("hex mixed case digits", 0xAbCdEf, 0XaBcDeF, 0xABCDEF)
  print ("big integer", 123456789012345678901234567890)
  print ("big hex", 0xFFFFFFFFFFFFFFFFFFFF)
  print ("big octal", 0o7777777777777777777777777)
  print ("int max", 9223372036854775807 :: Int)
  print ("int max hex", 0x7fffffffffffffff :: Int)
  print ("at Double", 5 :: Double, 0x10 :: Double, 0o10 :: Double)
  print ("negated", -1, -0x10, -0o10)
  print ("literal then identifier", let o7 = 100 in [0, o7])
  print ("literal then x-identifier", let xg = 5 in (+) 0 xg)
  print ("adjacent literals as args", max 0x10 0o10)
  print ("equalities", 0x10 == 16, 0o10 == 8, 010 == 10)
