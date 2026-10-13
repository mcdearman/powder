-- | The C preprocessor, as far as Haskell sources use it: lines kept or
-- left out by what is defined, and names that stand for text.
--
-- A source comes out with as many lines as it went in with. A line that
-- is a directive, or that is left out, is a blank one, so everything
-- that is kept is on the line it was on.
--
-- What is here: @#if@, @#ifdef@, @#ifndef@, @#elif@, @#else@, @#endif@,
-- @#define@ of a name or of a name with parameters, @#undef@ and
-- @#error@. A condition is whole numbers, @defined@, the comparisons,
-- arithmetic, @!@, @&&@ and @||@. What is not: @#include@, and @#@ and
-- @##@ in what a name stands for.
module Powder.Cpp (preprocess) where

import Data.Bifunctor (first)
import Data.Char (isAlpha, isAlphaNum, isDigit, isHexDigit, isSpace)
import Data.List (isPrefixOf, isSuffixOf)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Numeric (readHex)

-- | What a name stands for: text, or text with parameters to fill in.
data Macro = Macro (Maybe [String]) String

type Macros = Map.Map String Macro

-- | One @#if@ that is open: whether what is around it is kept, whether a
-- branch of it has been kept yet, and whether this branch is.
data Open = Open {around :: Bool, taken :: Bool, kept :: Bool}

-- | A source with its directives carried out, or the line something is
-- wrong at and what.
preprocess :: String -> Either (Int, String) String
preprocess source = finish <$> go predefined [] (zip [1 ..] (lines source))
  where
    finish ls = if "\n" `isSuffixOf` source || null source then unlines ls else init' (unlines ls)
    init' s = take (length s - 1) s
    predefined = Map.fromList [("__POWDER__", Macro Nothing "1")]

    go :: Macros -> [Open] -> [(Int, String)] -> Either (Int, String) [String]
    go _ [] [] = Right []
    go _ (_ : _) [] = Left (length (lines source), "an #if has no #endif")
    go macros open ((n, line) : rest)
      | '#' : directive <- line, not (shebang n line) = do
          -- A directive goes on over the lines that end in a backslash.
          let (more, after) = continued directive rest
              (word, argument) = break isSpace (dropWhile isSpace (concat (directive' directive : more)))
              blanks = "" : map (const "") more
              live = all kept open
              wrong message = Left (n, message)
          (macros', open') <- case word of
            "if" -> do
              v <- if live then truth n macros argument else pure False
              pure (macros, Open live v (live && v) : open)
            "ifdef" -> let v = Map.member (name argument) macros in pure (macros, Open live v (live && v) : open)
            "ifndef" -> let v = not (Map.member (name argument) macros) in pure (macros, Open live v (live && v) : open)
            "elif" -> case open of
              o : os
                | around o && not (taken o) -> do
                    v <- truth n macros argument
                    pure (macros, o {taken = v, kept = v} : os)
                | otherwise -> pure (macros, o {kept = False} : os)
              [] -> wrong "an #elif has no #if before it"
            "else" -> case open of
              o : os -> pure (macros, o {taken = True, kept = around o && not (taken o)} : os)
              [] -> wrong "an #else has no #if before it"
            "endif" -> case open of
              _ : os -> pure (macros, os)
              [] -> wrong "an #endif has no #if before it"
            "define" | live -> case define (dropWhile isSpace argument) of
              Just (x, macro) -> pure (Map.insert x macro macros, open)
              Nothing -> wrong "a #define names nothing"
            "undef" | live -> pure (Map.delete (name argument) macros, open)
            "error" | live -> wrong ("#error" ++ argument)
            "include" | live -> wrong "#include is not something this preprocessor does"
            _ -> pure (macros, open)
          (blanks ++) <$> go macros' open' after
      | all kept open = (expand macros line :) <$> go macros open rest
      | otherwise = ("" :) <$> go macros open rest

    -- The first line of a script says what runs it, and is no directive.
    shebang n line = n == (1 :: Int) && "#!" `isPrefixOf` line
    directive' d = if "\\" `isSuffixOf` d then init' d else d
    continued d rest
      | "\\" `isSuffixOf` d, (_, l) : more <- rest = let (ls, after) = continued l more in (directive' l : ls, after)
      | otherwise = ([], rest)
    name = takeWhile identifier . dropWhile isSpace

-- | Whether a character can be in a name.
identifier :: Char -> Bool
identifier c = isAlphaNum c || c == '_' || c == '\''

-- | What a @#define@ defines: a name, with parameters if a parenthesis
-- is hard against it, and the text it stands for.
define :: String -> Maybe (String, Macro)
define text = case span identifier text of
  ("", _) -> Nothing
  (x, '(' : after) ->
    let (parameters, body) = break (== ')') after
     in Just (x, Macro (Just (map trim (splitOn ',' parameters))) (trim (drop 1 body)))
  (x, body) -> Just (x, Macro Nothing (trim body))

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
  (a, _ : rest) -> a : splitOn c rest
  (a, []) -> [a | not (null (trim a))]

-- | A line with the names that stand for text replaced by it. What is
-- between double quotes is left as it is.
expand :: Macros -> String -> String
expand macros = go
  where
    go [] = []
    go s@(c : rest)
      | c == '"' = let (literal, after) = string rest in c : literal ++ go after
      | isAlpha c || c == '_' =
          let (x, after) = span identifier s
           in case Map.lookup x macros of
                Just (Macro Nothing body) -> expand (Map.delete x macros) body ++ go after
                Just (Macro (Just parameters) body)
                  | '(' : inside <- dropWhile isSpace after,
                    Just (arguments, after') <- arguments' inside ->
                      expand (Map.delete x macros) (fill (zip parameters (map (trim . go) arguments)) body) ++ go after'
                _ -> x ++ go after
      -- A number's letters are not a name.
      | isDigit c = let (number, after) = span identifier s in number ++ go after
      | otherwise = c : go rest
    -- To the quote that ends it, and what is after.
    string [] = ([], [])
    string ('\\' : c : rest) = let (a, b) = string rest in ('\\' : c : a, b)
    string ('"' : rest) = ("\"", rest)
    string (c : rest) = let (a, b) = string rest in (c : a, b)
    -- What is between the parentheses, at each comma not inside more.
    arguments' = collect (0 :: Int) "" []
    collect depth current done = \case
      [] -> Nothing
      ')' : rest
        | depth == 0 -> Just (reverse (reverse current : done), rest)
        | otherwise -> collect (depth - 1) (')' : current) done rest
      '(' : rest -> collect (depth + 1) ('(' : current) done rest
      ',' : rest | depth == 0 -> collect depth "" (reverse current : done) rest
      c : rest -> collect depth (c : current) done rest
    fill filling body = case body of
      [] -> []
      s@(c : rest)
        | isAlpha c || c == '_' -> let (x, after) = span identifier s in fromMaybe x (lookup x filling) ++ fill filling after
        | otherwise -> c : fill filling rest

-- | Whether a condition holds.
truth :: Int -> Macros -> String -> Either (Int, String) Bool
truth n macros text = case tokens (expand macros (definedIn text)) of
  Nothing -> Left (n, "the condition of an #if has something in it that is not understood")
  Just ts -> case orE ts of
    Just (v, []) -> Right (v /= 0)
    _ -> Left (n, "the condition of an #if is not understood")
  where
    -- Whether a name is defined is asked before any is replaced.
    definedIn s = case s of
      [] -> []
      c : rest
        | isAlpha c || c == '_' ->
            let (x, after) = span identifier s
             in if x == "defined"
                  then case dropWhile isSpace after of
                    '(' : inside -> let (y, after') = span identifier (dropWhile isSpace inside) in answer y ++ definedIn (drop 1 (dropWhile isSpace after'))
                    other -> let (y, after') = span identifier other in answer y ++ definedIn after'
                  else x ++ definedIn after
        | otherwise -> c : definedIn rest
    answer y = if Map.member y macros then " 1 " else " 0 "

data Token = Number Integer | Name String | Symbol String
  deriving (Eq)

tokens :: String -> Maybe [Token]
tokens = \case
  [] -> Just []
  s@(c : rest)
    | isSpace c -> tokens rest
    | '0' : x : more <- s, x `elem` "xX", (digits, after) <- span isHexDigit more, [(v, "")] <- readHex digits -> (Number v :) <$> tokens (suffix after)
    | isDigit c -> let (digits, after) = span isDigit s in (Number (read digits) :) <$> tokens (suffix after)
    | isAlpha c || c == '_' -> let (x, after) = span identifier s in (Name x :) <$> tokens after
    | two@[_, _] <- take 2 s, two `elem` ["||", "&&", "==", "!=", "<=", ">="] -> (Symbol two :) <$> tokens (drop 2 s)
    | c `elem` "!<>+-*/%()," -> (Symbol [c] :) <$> tokens rest
    | otherwise -> Nothing
  where
    suffix = dropWhile (`elem` "uUlL")

type Parser = [Token] -> Maybe (Integer, [Token])

-- | Operators that group to the left, between what binds tighter.
leftwards :: [(String, Integer -> Integer -> Maybe Integer)] -> Parser -> Parser
leftwards operators operand ts = operand ts >>= uncurry more
  where
    more left = \case
      Symbol o : rest | Just f <- lookup o operators -> do
        (right, rest') <- operand rest
        f left right >>= (`more` rest')
      rest -> Just (left, rest)

orE, andE, equal, compared, added, multiplied, unary, primary :: Parser
orE = leftwards [("||", \a b -> Just (flag (a /= 0 || b /= 0)))] andE
andE = leftwards [("&&", \a b -> Just (flag (a /= 0 && b /= 0)))] equal
equal = leftwards [("==", \a b -> Just (flag (a == b))), ("!=", \a b -> Just (flag (a /= b)))] compared
compared = leftwards [("<", \a b -> Just (flag (a < b))), ("<=", \a b -> Just (flag (a <= b))), (">", \a b -> Just (flag (a > b))), (">=", \a b -> Just (flag (a >= b)))] added
added = leftwards [("+", \a b -> Just (a + b)), ("-", \a b -> Just (a - b))] multiplied
multiplied = leftwards [("*", \a b -> Just (a * b)), ("/", divided quot), ("%", divided rem)] unary
  where
    divided f a b = if b == 0 then Nothing else Just (f a b)
unary = \case
  Symbol "!" : rest -> first (flag . (== 0)) <$> unary rest
  Symbol "-" : rest -> first negate <$> unary rest
  Symbol "+" : rest -> unary rest
  ts -> primary ts
primary = \case
  Number v : rest -> Just (v, rest)
  Symbol "(" : rest -> case orE rest of
    Just (v, Symbol ")" : after) -> Just (v, after)
    _ -> Nothing
  -- A name applied to numbers asks whether a package is new enough. The
  -- library here is taken to be the one GHC2024 came with.
  Name x : Symbol "(" : rest -> do
    (arguments, after) <- applied rest
    Just (if x == "MIN_VERSION_base" then flag (arguments <= [4, 20, 0]) else 0, after)
  -- A name that stands for nothing is nothing.
  Name _ : rest -> Just (0, rest)
  _ -> Nothing
  where
    applied ts = do
      (v, after) <- orE ts
      case after of
        Symbol "," : more -> first (v :) <$> applied more
        Symbol ")" : more -> Just ([v], more)
        _ -> Nothing

flag :: Bool -> Integer
flag b = if b then 1 else 0
