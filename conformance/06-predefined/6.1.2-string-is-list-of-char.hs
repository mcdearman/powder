-- Haskell 2010 Report 6.1.2: type String = [Char]; string literals are lists of characters; gaps and \&
countVowels :: String -> Int
countVowels s = length [c | c <- s, c `elem` "aeiou"]

firstChar :: [Char] -> Char
firstChar (c : _) = c
firstChar [] = '?'

asChars :: String -> [Char]
asChars s = s

main :: IO ()
main = do
  print ("literal equals list", "abc" == ['a', 'b', 'c'], "abc" == 'a' : 'b' : 'c' : [])
  print ("empty", "" == ([] :: [Char]), null "", length "")
  print ("synonym", asChars "xyz", firstChar "hello", firstChar "")
  print ("list functions", length "hello", reverse "hello", "ab" ++ "cd", head "xyz")
  print ("map over chars", map succ "HAL", countVowels "conformance")
  print ("cons pattern", case "hey" of { (c : cs) -> (c, cs); [] -> ('-', "") })
  print ("literal pattern", case "yes" of { "no" -> 0 :: Int; "yes" -> 1; _ -> 2 })
  print ("ordering", "abc" < "abd", "ab" < "abc", "" < "a", compare "b" "abc", "Z" < "a")
  print ("empty escape", "a\&b", length "a\&b", length "\&", "\&" == "")
  print ("gap", "abc\   \def", length "abc\   \def")
  print ("gap across lines", "one\
                             \two")
  print ("gap of only a newline and indent", "x\
     \y")
  print ("lines/unlines", lines "a\nb\n", unlines ["a", "b"], words " a  b ", unwords ["a", "b"])
  print ("char ops", succ 'a', pred 'b', fromEnum 'A', toEnum 97 :: Char, maximum "hello")
