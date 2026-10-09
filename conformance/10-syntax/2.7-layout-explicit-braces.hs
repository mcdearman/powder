-- Haskell 2010 Report 2.7 / 10.3: explicit braces and semicolons; inside them indentation is insignificant
module Main (main) where {

import Data.Char (toUpper);

data Shape = Circle Int
           | Square Int deriving Show;

area :: Shape -> Int;
area s = case s of {
   Circle r -> 3 * r * r;
 Square a -> a * a
};

f :: Int -> Int;
f x = y + z where {
y = x;
        z = 2 };

g :: Int -> Int;
g x = let { a = x;
  b = 1 } in a + b;

class Describe a where {
  describe :: a -> String;
  describe _ = "thing";
  name' :: a -> String
};

instance Describe Shape where {
name' (Circle _) = "circle";
     name' (Square _) = "square"
};

-- empty declarations and stray semicolons are allowed
;;

main :: IO ();
main = do {
  print ("area", area (Circle 1), area (Square 2));
 print ("f", f 1);
      print ("g", g 1);
  print ("class", describe (Circle 1), name' (Square 1));
  ; ;
  let { u = 1; v = 2; };
  print ("let with trailing semicolon", u + v);
  x <- return (map toUpper "abc");
  print ("bind", x);
  case x of { "ABC" -> print ("case", "matched"); _ -> print ("case", "other"); };
  print ("done", ())
}
}
