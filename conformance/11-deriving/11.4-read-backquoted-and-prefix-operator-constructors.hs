-- Haskell 2010 Report 11.4: derived Read accepts exactly the form derived Show produces: backquoted infix for a
-- constructor declared infix with an alphanumeric name, and `(:*:) 1 2` for an operator constructor declared prefix
module Main (main) where

infixr 5 `Foo`

infixl 6 :*:

infixr 5 :>

-- declared infix, backquoted name
data T = Int `Foo` Int deriving (Eq, Show, Read)

-- declared infix, default fixity
data U = Int `Baz` Int deriving (Eq, Show, Read)

-- operator name, declared prefix
data O = (:*:) Int Int deriving (Eq, Show, Read)

-- recursive, operator name declared prefix
data L = Nil | (:>) Int L deriving (Eq, Show, Read)

-- recursive, backquoted name declared infix
data Chain = End | Int `Link` Chain deriving (Eq, Show, Read)

infixr 5 `Link`

main :: IO ()
main = do
  print ("backquoted", read "1 `Foo` 2" :: T, read "(1 `Foo` 2)" :: T, read "  1   `Foo`   2  " :: T)
  print ("backquoted negative", read "(-1) `Foo` (-2)" :: T, read "-1 `Foo` -2" :: T)
  print ("backquoted prefix form is not read", null (reads "Foo 1 2" :: [(T, String)]))
  print ("backquoted remaining", reads "1 `Foo` 2 rest" :: [(T, String)])
  print ("backquoted in structures", read "Just (1 `Foo` 2)" :: Maybe T, read "[1 `Foo` 2,3 `Foo` 4]" :: [T])
  print ("backquoted default fixity", read "1 `Baz` 2" :: U, read "(-1) `Baz` (-2)" :: U, read "Just (1 `Baz` 2)" :: Maybe U)
  print ("backquoted precedence", null (readsPrec 6 "1 `Foo` 2" :: [(T, String)]), readsPrec 5 "1 `Foo` 2" :: [(T, String)], readsPrec 6 "(1 `Foo` 2)" :: [(T, String)])
  print ("prefix operator", read "(:*:) 1 2" :: O, read "((:*:) 1 2)" :: O, read "  (:*:)   1   2  " :: O)
  print ("prefix operator negative", read "(:*:) (-1) (-2)" :: O)
  print ("prefix operator: infix form is not read", null (reads "1 :*: 2" :: [(O, String)]))
  print ("prefix operator precedence", null (readsPrec 11 "(:*:) 1 2" :: [(O, String)]), readsPrec 10 "(:*:) 1 2" :: [(O, String)], readsPrec 11 "((:*:) 1 2)" :: [(O, String)])
  print ("prefix operator in structures", read "Just ((:*:) 1 2)" :: Maybe O, read "[(:*:) 1 2,(:*:) 3 4]" :: [O])
  print ("recursive prefix operator", read "Nil" :: L, read "(:>) 1 Nil" :: L, read "(:>) 1 ((:>) 2 Nil)" :: L)
  print ("recursive backquoted", read "End" :: Chain, read "1 `Link` End" :: Chain, read "1 `Link` (2 `Link` End)" :: Chain)
  print ("recursive backquoted needs parentheses", null [() | (_, "") <- reads "1 `Link` 2 `Link` End" :: [(Chain, String)]])
  print ("read . show", read (show (1 `Foo` 2)) == 1 `Foo` 2, read (show ((-1) `Baz` (-2))) == (-1) `Baz` (-2), read (show ((-1) :*: 2)) == (-1) :*: 2)
  print ("read . show recursive", read (show (1 :> 2 :> 3 :> Nil)) == 1 :> 2 :> 3 :> Nil, read (show (1 `Link` 2 `Link` End)) == 1 `Link` 2 `Link` End)
