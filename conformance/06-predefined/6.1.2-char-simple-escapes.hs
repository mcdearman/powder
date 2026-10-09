-- Haskell 2010 Report 6.1.2 (and 2.6): single-character escapes \a \b \f \n \r \t \v \\ \" \'
main :: IO ()
main = do
  print ("string codes", map fromEnum "\a\b\f\n\r\t\v")
  print ("backslash and quotes", map fromEnum "\\\"\'")
  print ("char codes 1", map fromEnum ['\a', '\b', '\f', '\n', '\r', '\t', '\v'])
  print ("char codes 2", map fromEnum ['\\', '\'', '\"', '"'])
  print ("apostrophe in string", map fromEnum "'")
  print ("lengths", length "\n", length "\\n", length "\\\\", length "\"\"")
  print ("names 1", '\a' == '\BEL', '\b' == '\BS', '\f' == '\FF')
  print ("names 2", '\n' == '\LF', '\r' == '\CR', '\t' == '\HT', '\v' == '\VT')
  print ("numeric", '\a' == '\7', '\n' == '\10', '\\' == '\92', '\'' == '\39')
  print ("space is a char", fromEnum ' ', ' ' == '\SP', ' ' == '\32')
  print ("escaped vs unescaped quote", '\"' == '"', "\'" == "'")
  putStrLn "tab:[\t]"
  putStrLn "backslash:[\\] dquote:[\"] squote:[\']"
  putStr "line one\nline two\n"
