module Zoo where

infixl 6 <+>

data Animal = Cat | Dog | Emu deriving (Show, Eq, Ord, Enum, Bounded)

data Cage = Cage { occupant :: Animal, number :: Int } deriving (Show, Eq)

newtype Keeper = Keeper String deriving Show

type Roster = [(Keeper, Animal)]

class Noisy a where
  noise :: a -> String
  loud :: a -> String
  loud x = noise x ++ "!"

instance Noisy Animal where
  noise Cat = "meow"
  noise Dog = "woof"
  noise Emu = "boom"

legs :: Animal -> Int
legs Emu = 2
legs _ = 4

feed :: Animal -> String
feed a = "feeding " ++ show a

(<+>) :: Int -> Int -> Int
a <+> b = a + b

zooName :: String
zooName = "Zoo"
