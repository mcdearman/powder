module Right (shared, twice, Token(Token), Weigh(weigh), rightOnly, rightToken) where

import Base

rightOnly :: Int
rightOnly = twice 100

rightToken :: Token
rightToken = Token 3
