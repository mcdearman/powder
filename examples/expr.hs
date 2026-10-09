-- Data types, nested patterns, case, and mutual recursion.

data Expr
  = Num Int
  | Add Expr Expr
  | Mul Expr Expr
  | Neg Expr
  | Var Char
  | Let Char Expr Expr

eval :: [(Char, Int)] -> Expr -> Maybe Int
eval _ (Num n) = Just n
eval env (Add a b) = lift2 (+) (eval env a) (eval env b)
eval env (Mul a b) = lift2 (*) (eval env a) (eval env b)
eval env (Neg a) = lift2 (-) (Just 0) (eval env a)
eval env (Var x) = lookup x env
eval env (Let x e body) = case eval env e of
  Nothing -> Nothing
  Just v -> eval ((x, v) : env) body

lift2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
lift2 f (Just a) (Just b) = Just (f a b)
lift2 _ _ _ = Nothing

simplify :: Expr -> Expr
simplify (Add (Num 0) e) = simplify e
simplify (Add e (Num 0)) = simplify e
simplify (Mul (Num 1) e) = simplify e
simplify (Mul e (Num 1)) = simplify e
simplify (Mul (Num 0) _) = Num 0
simplify (Add a b) = Add (simplify a) (simplify b)
simplify (Mul a b) = Mul (simplify a) (simplify b)
simplify (Neg (Neg e)) = simplify e
simplify e = e

render :: Expr -> String
render (Num n) = show n
render (Add a b) = "(" ++ render a ++ " + " ++ render b ++ ")"
render (Mul a b) = "(" ++ render a ++ " * " ++ render b ++ ")"
render (Neg a) = "-" ++ render a
render (Var x) = [x]
render (Let x e body) = "let " ++ [x] ++ " = " ++ render e ++ " in " ++ render body

parity :: Int -> String
parity n = if isEven n then "even" else "odd"
  where
    isEven 0 = True
    isEven k = isOdd (k - 1)
    isOdd 0 = False
    isOdd k = isEven (k - 1)

report :: Expr -> IO ()
report e = do
  putStr (render e ++ " = ")
  case eval [] e of
    Nothing -> putStrLn "?"
    Just v -> putStrLn (show v ++ ", which is " ++ parity (abs v))

main :: IO ()
main = do
  let e = Let 'x' (Add (Num 3) (Num 4)) (Mul (Var 'x') (Neg (Var 'x')))
  report e
  report (Add (Var 'y') (Num 1))
  report (simplify (Add (Mul (Num 1) (Num 6)) (Mul (Num 0) (Var 'z'))))
