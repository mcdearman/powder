module Left (module Base, leftOnly, leftToken) where

import Base

leftOnly :: Int
leftOnly = twice 10

leftToken :: Token
leftToken = Token 3
