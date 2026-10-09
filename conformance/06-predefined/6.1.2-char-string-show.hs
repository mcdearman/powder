-- Haskell 2010 Report 6.1.2 / 6.3.3: show of characters and strings (showLitChar escapes, \& protection)
line :: Show a => String -> a -> IO ()
line label x = putStrLn (label ++ ": " ++ show x)

main :: IO ()
main = do
  line "plain char" 'a'
  line "newline char" '\n'
  line "tab char" '\t'
  line "single quote char" '\''
  line "double quote char" '"'
  line "backslash char" '\\'
  line "NUL char" '\0'
  line "ESC char" '\ESC'
  line "DEL char" '\DEL'
  line "char 128" '\128'
  line "char 200" '\200'
  line "max char" (maxBound :: Char)
  line "plain string" "hello"
  line "empty string" ""
  line "string with double quote" "say \"hi\""
  line "string with single quote" "it's"
  line "string with backslash" "a\\b"
  line "simple escapes" "\a\b\f\n\r\t\v"
  line "control names" "\NUL\SOH\STX\ESC\US\DEL"
  line "SO then H" "\SO\&H"
  line "SOH" "\SOH"
  line "SO then other letter" "\SOA"
  line "numeric then digit" "\200\&1"
  line "numeric then letter" "\200a"
  line "numeric then space digit" "\200 1"
  line "large code point" "\1234\&5\x10FFFF"
  line "list of chars is a string" ['a', 'b']
  line "list of strings" ["a", "b\n"]
  line "char in tuple" ('x', "y")
  line "Maybe String" (Just "z")
  putStrLn (showString "shows: " (shows 'q' (shows "r" "")))
  print (map show "a'\"", show (show "x"))
