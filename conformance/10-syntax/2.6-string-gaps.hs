-- Haskell 2010 Report 2.6: string gaps: backslash, whitespace (incl. newlines), backslash is ignored
module Main (main) where

s1 :: String
s1 = "abc\
     \def"

s2 :: String
s2 = "one \
        \two \
   \three"

s3 :: String
s3 = "gap on one line:\   \end"

s4 :: String
s4 = "line1\n\
     \line2\n\
     \line3"

s5 :: String
s5 = "\
     \starts with a gap"

s6 :: String
s6 = "ends with a gap\
     \"

s7 :: String
s7 = "num\65\
     \66"

s8 :: String
s8 = "tab gap:\	\end"

s9 :: String
s9 = "blank line in gap\

     \ok"

main :: IO ()
main = do
  print ("s1", s1)
  print ("s2", s2)
  print ("s3", s3)
  print ("s4", s4, length (lines s4))
  print ("s5", s5)
  print ("s6", s6)
  print ("s7 gap separates numeric escape from digits", s7, length s7)
  print ("s8", s8)
  print ("s9", s9)
  print ("inline gap equals none", "a\ \b" == "ab", length "\ \")
  print ("gap in argument", length "12\
                                    \34")
