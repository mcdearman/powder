module Parts.Shapes where

data Shape = Circle Int | Square Int

area :: Shape -> Int
area (Circle r) = 3 * r * r
area (Square s) = s * s

-- The same name as a function of the prelude's and of Main's.
length :: Shape -> Int
length (Circle r) = 6 * r
length (Square s) = 4 * s
