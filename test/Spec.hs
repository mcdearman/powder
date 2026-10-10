-- | Every test of the compiler and its runtimes.
module Main (main) where

import qualified ConformanceSpec
import qualified DiagnosticsSpec
import qualified EntrySpec
import qualified ExamplesSpec
import qualified HazySpec
import qualified ResolveSpec
import qualified SyntaxSpec
import Test.Hspec
import qualified TypecheckSpec

main :: IO ()
main = hspec $ do
  describe "what is typed at the prompt" EntrySpec.spec
  describe "syntax" SyntaxSpec.spec
  describe "name resolution" ResolveSpec.spec
  describe "types" TypecheckSpec.spec
  describe "diagnostics" DiagnosticsSpec.spec
  describe "the examples" ExamplesSpec.spec
  describe "Haskell 2010 conformance" ConformanceSpec.spec
  describe "Hazy's tests" HazySpec.spec
