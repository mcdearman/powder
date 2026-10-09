-- | How the prompt reads an entry: when Enter submits, how the next line
-- is indented, and what completes where.
module EntrySpec (spec) where

import Control.Monad (forM_)
import Data.Maybe (isNothing)
import Entry
import Test.Hspec

spec :: Spec
spec = forM_ tests (\(name, ok) -> it name (ok `shouldBe` True))

-- | What plain Enter does to a buffer with the cursor at its end: 'Nothing'
-- is "submit", and otherwise the buffer after the line break and indent.
pressEnter :: String -> Maybe String
pressEnter buf = (buf ++) <$> lineBreak False buf (length buf)

-- | The same for a key that asks for a line break by name.
pressAltEnter :: String -> Maybe String
pressAltEnter buf = (buf ++) <$> lineBreak True buf (length buf)

tests :: [(String, Bool)]
tests =
  [ -- Submitting.
    ("enter submits a finished entry", all (isNothing . pressEnter) ["1 + 2", "x = 1", ":comb map", "f (1, 2)", "\"(\"", ""]),
    ("an unfinished first line stays open", pressEnter "1 +" == Just "1 +\n  "),
    ("so does one that does not parse at all", pressEnter "1 + )" == Just "1 + )\n"),
    ("a blank line submits whatever is there", isNothing (pressEnter "1 + )\n") && isNothing (pressEnter "f x =\n  ")),
    ("an entry is submitted early once it parses", isNothing (pressEnter "f x =\n  x + 1")),
    ("but not with something dangling", pressEnter "f x =\n  x +" == Just "f x =\n  x +\n    "),
    ("nor with a bracket open", pressEnter "f x =\n  g (x," == Just "f x =\n  g (x,\n    "),
    ("an expression said to have a type does not wait", all (isNothing . pressEnter) ["1 + 2 :: Int", "(minBound :: Int)", "[1 :: Int ..] !! 2"]),
    ("a signature waits for its equations", pressEnter "fact :: Integer -> Integer" == Just "fact :: Integer -> Integer\n"),
    ("and takes all of them, to the blank line", pressEnter "fact :: Integer -> Integer\nfact 0 = 1" == Just "fact :: Integer -> Integer\nfact 0 = 1\n"),
    ("which ends it", isNothing (pressEnter "fact :: Integer -> Integer\nfact 0 = 1\nfact n = n * fact (n - 1)\n")),
    ("guards wait for the blank line", pressEnter "sign n\n  | n < 0 = 0 - 1" == Just "sign n\n  | n < 0 = 0 - 1\n  "),
    ("a command is never continued", isNothing (pressEnter ":comb (1 +")),
    -- Indenting.
    ("a body sits one unit inside", pressEnter "f x =" == Just "f x =\n  "),
    ("alternatives sit one unit inside the case", pressEnter "f x = case x of" == Just "f x = case x of\n  "),
    ("an alternative's body hangs under it", pressEnter "f x = case x of\n  Just y ->" == Just "f x = case x of\n  Just y ->\n    "),
    ("a do block's statements line up", pressEnter "main = do\n  print 1\n  print (2 +" == Just "main = do\n  print 1\n  print (2 +\n    "),
    ("the indent is for where the cursor is", lineBreak False "f x =\n  g (x" 5 == Just "\n  "),
    -- Line breaks asked for by name.
    ("alt-enter breaks a finished line", pressAltEnter "fact 0 = 1" == Just "fact 0 = 1\n"),
    ("after a head, for the guards under it", pressAltEnter "sign n" == Just "sign n\n  "),
    ("and keeps the indent it is at", pressAltEnter "main = do\n  print 1" == Just "main = do\n  print 1\n  "),
    ("an entry built with it submits when it parses", isNothing (pressEnter "fact 0 = 1\nfact n = n * fact (n - 1)")),
    -- Completion.
    ("a word starts after the last separator", wordStart "map (fol" 8 == 5 && wordStart "foldl'" 6 == 0),
    ("an expression wants values and constructors", position "map (fol" == Term && position "" == Term),
    ("a signature wants types", position "f :: Int -> In" == Type),
    ("so does a later line that is one", position "x = 1\nf :: In" == Type),
    ("an equation under a signature is back to terms", position "f :: Int\nf x = fol" == Term),
    ("a data declaration's fields are types", position "data T = A In" == Type),
    ("its own names are new", position "data Tr" == Naming && position "data T = No" == Naming && position "data T = A | B" == Naming)
  ]
