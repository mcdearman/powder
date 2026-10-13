{-# LANGUAGE GHC2021 #-}
-- Control.Monad.State, Reader, Writer and Except, and the monads they are made of: something kept, something
-- read, something added to and a way to stop, alone and one over another
module Main (main) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.ST
import Control.Monad.State
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Writer
import Data.Char (isDigit, isSpace)
import Data.Functor.Identity (Identity (..))
import qualified Data.Map as Map
import Data.STRef

-- Something kept: a counter, and labels made from it.
fresh :: State Int Int
fresh = do
  n <- get
  put (n + 1)
  return n

label :: String -> State Int String
label prefix = (\n -> prefix ++ show n) <$> fresh

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show)

numbered :: Tree a -> State Int (Tree (Int, a))
numbered Leaf = return Leaf
numbered (Node l x r) = do
  l' <- numbered l
  n <- fresh
  r' <- numbered r
  return (Node l' (n, x) r')

-- Written for any monad that keeps a number.
bump :: MonadState Int m => Int -> m Int
bump by = do
  modify (+ by)
  gets (* 2)

-- Something read: an environment of names, with more of them inside a let.
data Expr = Lit Int | Var String | Add Expr Expr | Let String Expr Expr | Div Expr Expr

eval :: Expr -> Reader (Map.Map String Int) Int
eval (Lit n) = return n
eval (Var x) = asks (Map.findWithDefault 0 x)
eval (Add a b) = (+) <$> eval a <*> eval b
eval (Let x e body) = do
  v <- eval e
  local (Map.insert x v) (eval body)
eval (Div a b) = div <$> eval a <*> eval b

-- The same with a way to stop, a count of steps and a log, each from a monad under the one before.
type Eval = ReaderT (Map.Map String Int) (ExceptT String (WriterT [String] (State Int)))

step :: Eval ()
step = modify (+ 1)

evalM :: Expr -> Eval Int
evalM e = step >> case e of
  Lit n -> return n
  Var x -> do
    found <- asks (Map.lookup x)
    case found of
      Just v -> return v
      Nothing -> throwError ("unbound: " ++ x)
  Add a b -> (+) <$> evalM a <*> evalM b
  Let x bound body -> do
    v <- evalM bound
    tell [x ++ " = " ++ show v]
    local (Map.insert x v) (evalM body)
  Div a b -> do
    x <- evalM a
    y <- evalM b
    when (y == 0) (throwError "divide by zero")
    return (x `div` y)

runEval :: Expr -> ((Either String Int, [String]), Int)
runEval e = runState (runWriterT (runExceptT (runReaderT (evalM e) Map.empty))) 0

-- A parser: what is left to read is kept, and there may be no result.
type Parser = StateT String Maybe

item :: Parser Char
item = do
  s <- get
  case s of
    c : rest -> put rest >> return c
    [] -> lift Nothing

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
  c <- item
  if p c then return c else lift Nothing

many1 :: Parser a -> Parser [a]
many1 p = do
  x <- p
  xs <- many0 p
  return (x : xs)

many0 :: Parser a -> Parser [a]
many0 p = StateT (\s -> case runStateT p s of
  Nothing -> Just ([], s)
  Just (x, rest) -> fmap (\(xs, end) -> (x : xs, end)) (runStateT (many0 p) rest))

number :: Parser Int
number = read <$> many1 (satisfy isDigit) <* many0 (satisfy isSpace)

sumOf :: Parser Int
sumOf = do
  n <- number
  rest <- many0 (satisfy (== '+') >> many0 (satisfy isSpace) >> number)
  return (n + sum rest)

-- Changing things no one else can see.
sumST :: [Int] -> Int
sumST xs = runST $ do
  total <- newSTRef 0
  mapM_ (\x -> modifySTRef total (+ x)) xs
  readSTRef total

fibST :: Int -> Integer
fibST n = runST $ do
  a <- newSTRef 0
  b <- newSTRef 1
  replicateM_ n $ do
    x <- readSTRef a
    y <- readSTRef b
    writeSTRef a y
    writeSTRef b $! x + y
  readSTRef a

collatz :: Int -> Writer [Int] Int
collatz 1 = tell [1] >> return 0
collatz n = do
  tell [n]
  steps <- collatz (if even n then n `div` 2 else 3 * n + 1)
  return (steps + 1)

safeDiv :: Int -> Int -> Except String Int
safeDiv _ 0 = throwError "nothing is divided by zero"
safeDiv a b = return (a `div` b)

main :: IO ()
main = do
  print (runState fresh 10, evalState (replicateM 3 fresh) 5, execState (mapM_ (const fresh) "abcd") 0)
  print (evalState (mapM label ["a", "b", "c"]) 1)
  print (evalState (numbered (Node (Node Leaf 'a' Leaf) 'b' (Node Leaf 'c' Leaf))) 0)
  print (runState (bump 3) 4, runState (modify' (* 2) >> gets show) 21, runState (state (\s -> (s * 2, s + 1))) 5)
  print (runState (withState (+ 1) get) 0, runIdentity (evalStateT (bump 1) 1))
  print (runState (zipWithM (\i x -> modify (+ x) >> gets (* i)) [1 ..] [10, 20, 30]) 0)
  let program = Let "x" (Add (Lit 3) (Lit 4)) (Let "y" (Add (Var "x") (Var "x")) (Add (Var "y") (Var "z")))
  print (runReader (eval program) (Map.fromList [("z", 100)]), runReader (eval (Var "nowhere")) Map.empty)
  print (runReader (local (* 2) (asks (+ 1))) 10, runReader (reader show >>= \s -> asks ((s ++) . show . negate)) 7)
  print (runEval (Let "x" (Lit 6) (Div (Var "x") (Lit 2))))
  print (runEval (Let "x" (Lit 6) (Div (Var "x") (Add (Var "x") (Lit (-6))))))
  print (runEval (Add (Lit 1) (Var "y")))
  print (runStateT sumOf "12 + 30 +  8", runStateT sumOf "7", runStateT sumOf "+", evalStateT number "42abc")
  print (sumST [1 .. 100], fibST 90, map fibST [0 .. 10])
  print (runWriter (collatz 6), execWriter (mapM_ (\x -> when (odd x) (tell [x])) [1 .. 9 :: Int]))
  print (runWriter (listen (tell "ab" >> tell "cd" >> return 'x')), runWriter (censor reverse (tell "abc" >> return ())), runWriter (listens length (tell [1, 2, 3 :: Int])))
  print (runExcept (safeDiv 10 2), runExcept (safeDiv 1 0), runExcept (safeDiv 1 0 `catchError` (\e -> return (length e))))
  print (runExcept (mapM (safeDiv 100) [1, 2, 5]), runExcept (mapM (safeDiv 100) [1, 0, 5]), runExcept (withExcept length (safeDiv 1 0)))
  print (either (++ "!") show (Left "plain" `catchError` (\e -> Left (e ++ " again")) :: Either String Int), (throwError 'x' :: Either Char ()), runExcept (tryError (safeDiv 1 0)))
  -- One over IO, which what is under it does.
  n <- execStateT (mapM_ (\x -> modify (+ x) >> get >>= lift . print) [1, 2, 3 :: Int]) 0
  r <- runReaderT (do x <- ask; liftIO (putStrLn ("read " ++ x)); local (++ "!") (asks length)) "this"
  e <- runExceptT (do liftIO (putStrLn "before"); _ <- throwError "stopped"; liftIO (putStrLn "after")) :: IO (Either String ())
  (m, w) <- runWriterT (do tell ["one"]; lift (putStrLn "in the middle"); tell ["two"]; return (3 :: Int))
  j <- runMaybeT (do x <- MaybeT (return (Just 1)); lift (print (x :: Int)); MaybeT (return Nothing) :: MaybeT IO ())
  print (n, r, e, m, w, j)
