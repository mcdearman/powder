-- What a type variable is applied to must be the same: nothing says what the variable's type has of it
import Data.Coerce (coerce)

newtype Age = Age Int

wrong :: f Age -> f Int
wrong = coerce

main :: IO ()
main = return ()
