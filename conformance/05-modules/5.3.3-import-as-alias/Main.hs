-- Haskell 2010 Report 5.3.3: `as` renames the qualifier; without `qualified` the unqualified names are in scope too;
-- two modules may share an alias as long as referenced names are unambiguous
module Main (main) where

import Zoo as Z
import qualified Zoo as Park
import qualified Data.List as Util
import qualified Data.Char as Util
import qualified Data.Maybe as Data.Opt

main :: IO ()
main = do
  print ("unqualified and aliased", legs Cat, Z.legs Z.Cat, Park.legs Park.Dog)
  print ("alias operator", 1 Z.<+> 2, 1 Park.<+> 2, 1 <+> 2)
  print ("same entity through different qualifiers", Z.Cat == Park.Cat, [Cat ..] == [Park.Cat ..])
  print ("shared alias for two modules", Util.sort "cab", Util.toUpper 'x', Util.isDigit '7')
  print ("dotted alias", Data.Opt.fromMaybe 0 (Just 1), Data.Opt.isNothing (Just 'x'))
  print ("alias type names", (Z.Cage Park.Emu 1 :: Park.Cage))
  print ("alias class method", Z.noise Emu, Park.loud Emu)
