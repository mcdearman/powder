-- Control.Applicative: Alternative, with some, many and optional, for Maybe, lists and a parser written here
import Control.Applicative

newtype Parser a = Parser (String -> Maybe (a, String))

runParser :: Parser a -> String -> Maybe (a, String)
runParser (Parser p) = p

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s -> case p s of
    Nothing -> Nothing
    Just (a, rest) -> Just (f a, rest)

instance Applicative Parser where
  pure x = Parser $ \s -> Just (x, s)
  Parser pf <*> Parser pa = Parser $ \s -> case pf s of
    Nothing -> Nothing
    Just (f, rest) -> case pa rest of
      Nothing -> Nothing
      Just (a, rest') -> Just (f a, rest')

instance Alternative Parser where
  empty = Parser (const Nothing)
  Parser p <|> Parser q = Parser $ \s -> case p s of
    Nothing -> q s
    r -> r

sat :: (Char -> Bool) -> Parser Char
sat p = Parser $ \s -> case s of
  c : cs | p c -> Just (c, cs)
  _ -> Nothing

char :: Char -> Parser Char
char c = sat (== c)

digit :: Parser Char
digit = sat (`elem` "0123456789")

number :: Parser Int
number = read <$> some digit

-- Sums of products of numbers and bracketed sums.
expr :: Parser Int
expr = (+) <$> term <* char '+' <*> expr <|> term

term :: Parser Int
term = (*) <$> factor <* char '*' <*> term <|> factor

factor :: Parser Int
factor = number <|> char '(' *> expr <* char ')'

main :: IO ()
main = do
  putStrLn ("maybe: " ++ show (Just 1 <|> Nothing, Nothing <|> Just (2 :: Int), Just 1 <|> Just (2 :: Int)))
  putStrLn ("maybe-empty: " ++ show (empty :: Maybe Int))
  putStrLn ("list: " ++ show ([1, 2] <|> [3 :: Int], empty <|> "ab"))
  putStrLn ("number: " ++ show (runParser number "123abc"))
  putStrLn ("no-number: " ++ show (runParser number "abc"))
  putStrLn ("many: " ++ show (runParser (many digit) "abc"))
  putStrLn ("many-some: " ++ show (runParser (many digit) "12a"))
  putStrLn ("optional: " ++ show (runParser (optional (char '-')) "-1", runParser (optional (char '-')) "1"))
  putStrLn ("expr: " ++ show (runParser expr "2*(3+4)+5*6"))
  putStrLn ("expr-rest: " ++ show (runParser expr "1+2)"))
  putStrLn ("pair: " ++ show (runParser ((,) <$> number <* char ',' <*> number) "12,34rest"))
