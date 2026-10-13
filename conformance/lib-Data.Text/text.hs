{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
-- Data.Text: text as a type of its own, with what the text package has for it
module Main (main) where

import Data.Char (isDigit, isSpace, isUpper, toUpper)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as TIO

sample :: Text
sample = "  The quick brown fox, the lazy dog.  "

csv :: Text
csv = "name,age,,city\nAda,36,,London\nBo,,x,"

main :: IO ()
main = do
  print (sample, T.pack "packed", T.unpack "unpacked", T.singleton 'x', T.empty, T.show (42 :: Int, True))
  print (T.cons '>' "abc", T.snoc "abc" '<', T.append "ab" "cd", "ab" <> "cd" <> mempty, mconcat ["x", "y", "z" :: Text])
  print (T.uncons "abc", T.uncons "", T.unsnoc "abc", T.unsnoc "", T.head "abc", T.last "abc", T.tail "abc", T.init "abc")
  print (T.null "", T.null "a", T.length sample, T.compareLength "abc" 2, T.compareLength "abc" 3, T.compareLength "abc" 4)
  print (T.map toUpper "shout", T.intercalate ", " ["a", "b", "c"], T.intercalate "-" [], T.intersperse '.' "abc", T.transpose ["abc", "de", "f"], T.reverse "stressed")
  print (T.replace "the" "a" sample, T.replace "o" "0" "foo boo", T.replace "aa" "b" "aaaaa", T.replace "x" "" "axbxc", T.replace "ab" "abab" "ab")
  print (T.toLower "MiXeD 123", T.toUpper "MiXeD 123", T.toTitle "the quick  brown-fox isn't here", T.toCaseFold "Straße ABC")
  print (T.justifyLeft 7 '.' "ab", T.justifyRight 7 '.' "ab", T.center 7 '.' "ab", T.center 8 '.' "abc", T.justifyLeft 1 '.' "abc", T.center 2 '*' "abc")
  print (T.foldl (\n c -> n * 10 + fromEnum c - 48) 0 "1234", T.foldl' (flip (:)) [] "abc", T.foldr (:) [] "abc", T.concat ["ab", "", "cd"], T.concatMap (\c -> T.pack [c, c]) "abc")
  print (T.any isDigit "ab1", T.any isDigit "abc", T.all isDigit "123", T.all isDigit "", T.maximum "hello", T.minimum "hello")
  print (T.take 3 "abcdef", T.take 9 "abc", T.take (-1) "abc", T.takeEnd 2 "abcdef", T.drop 3 "abcdef", T.dropEnd 2 "abcdef", T.dropEnd 9 "abc", T.takeEnd 9 "abc")
  print (T.takeWhile isDigit "123abc456", T.takeWhileEnd isDigit "123abc456", T.dropWhile isDigit "123abc456", T.dropWhileEnd isDigit "123abc456", T.dropAround isDigit "123abc456")
  print (T.strip sample, T.stripStart sample, T.stripEnd sample, T.strip " \t\n ", T.strip "none")
  print (T.splitAt 3 "abcdef", T.splitAt 0 "abc", T.splitAt 9 "abc", T.break (== ',') "ab,cd,ef", T.span isDigit "12ab", T.break isSpace "nospace")
  print (T.breakOn "::" "a::b::c", T.breakOn "/" "nothing", T.breakOnEnd "::" "a::b::c", T.breakOnEnd "/" "nothing", T.breakOnAll "::" "a::b::c", T.breakOnAll "aa" "aaaa", T.breakOnAll "/" "x")
  print (T.group "aabccc", T.groupBy (\a b -> isDigit a == isDigit b) "ab12cd3", T.inits "abc", T.tails "abc")
  print (T.splitOn "," "a,b,,c", T.splitOn "," "", T.splitOn ", " "a, b,c, d", T.splitOn "aa" "aaaaa", T.splitOn "x" "x", T.splitOn "::" "::a::")
  print (T.split (== ',') "a,b,,c", T.split isSpace " a  b ", T.split (== 'x') "", T.chunksOf 3 "abcdefgh", T.chunksOf 0 "abc", T.chunksOf 5 "")
  print (T.lines csv, T.lines "one\n\ntwo\n", T.lines "", T.words sample, T.words " \n ", T.unlines ["a", "b"], T.unwords ["a", "b", "c"])
  print (T.isPrefixOf "ab" "abc", T.isPrefixOf "" "abc", T.isPrefixOf "abcd" "abc", T.isSuffixOf "bc" "abc", T.isInfixOf "quick" sample, T.isInfixOf "slow" sample, T.isInfixOf "" "")
  print (T.stripPrefix "foo" "foobar", T.stripPrefix "bar" "foobar", T.stripSuffix "bar" "foobar", T.stripSuffix "foo" "foobar", T.stripPrefix "" "x")
  print (T.commonPrefixes "foobar" "fooquux", T.commonPrefixes "veeble" "fetzer", T.commonPrefixes "same" "same", T.commonPrefixes "" "x")
  print (T.filter isUpper "Hello World", T.find isDigit "ab3c4", T.find isDigit "abc", T.elem 'q' sample, T.elem 'z' sample, T.partition isDigit "a1b2c3")
  print (T.index "abc" 1, T.findIndex isDigit "ab3", T.findIndex isDigit "abc", T.count "the" sample, T.count "aa" "aaaaa", T.count "x" "")
  print (T.zip "abc" "12", T.zipWith max "adz" "bcy", T.replicate 3 "ab", T.replicate 0 "ab", T.replicate 2 "")
  print ("abc" == ("abc" :: Text), "abc" < ("abd" :: Text), compare ("b" :: Text) "a", maximum ["pear", "apple", "zebra" :: Text], (read "\"read\\tin\"" :: Text))
  print (show ("quote\"d\n" :: Text), showsPrec 11 ("in" :: Text) "", [T.pack (show n) | n <- [1 .. 3 :: Int]])
  -- Characters of two, three and four bytes: counted, cut and found as characters.
  let wide = T.pack "na\239ve caf\233 \955\8594\26085 \128512!" :: Text
  print (wide, T.length wide, T.unpack wide == "na\239ve caf\233 \955\8594\26085 \128512!", T.take 3 wide, T.drop 11 wide, T.takeEnd 2 wide, T.dropEnd 2 wide, T.splitAt 9 wide)
  print (T.reverse wide, T.toUpper wide, T.words wide, T.breakOn "\955" wide, T.splitOn "\8594" wide, T.count "\233" wide, T.isInfixOf "\8594\26085" wide, T.index wide 2, T.last wide, T.unsnoc wide)
  print (T.inits (T.take 3 (T.drop 10 wide)), T.tails (T.takeEnd 3 wide), T.group "\955\955\8594\8594", T.center 9 '\8594' "\955", T.replicate 2 (T.takeEnd 2 wide), T.strip "\160  x \160", T.pack "\55357" == T.singleton '\65533')
  print (compare ("\233" :: Text) "z", compare ("\26085" :: Text) "\128512", T.pack "ab\0cd" == "ab\0cd", T.copy (T.take 4 wide) == "na\239v", T.commonPrefixes "caf\233" "caf\233ine", T.dropWhileEnd (> 'z') wide, T.takeWhileEnd (/= ' ') wide)
  print (map T.length (T.chunksOf 4 wide), T.lines "\955\n\n\8594\n", T.justifyRight 4 '\233' "\955", T.intercalate "\8594" ["a", "\955", ""], T.stripSuffix "\128512!" wide, T.stripPrefix "na\239" wide, T.breakOnEnd " " wide)
  -- A table from the lines above: each row's cells, padded.
  let rows = map (T.splitOn ",") (T.lines csv)
      widths = [maximum [T.length (row !! i) | row <- rows] | i <- [0 .. 3]]
  mapM_ (TIO.putStrLn . T.intercalate " | " . zipWith (\w cell -> T.justifyLeft w ' ' cell) widths) rows
  TIO.putStr "no newline"
  TIO.putStrLn ""
  TIO.putStrLn (T.unwords (map (T.center 5 '*') ["a", "bb", "ccc"]))
