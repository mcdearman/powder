{-# LANGUAGE DisambiguateRecordFields #-}
-- DisambiguateRecordFields: where a record is built or matched, a field that two modules have is the constructor's
module Main (main) where

import Cat
import Dog

catName :: Cat -> String
catName Cat {name = n} = n

dogYears :: Dog -> Int
dogYears Dog {age = a} = a * 7

main :: IO ()
main = do
  let c = Cat {name = "Tom", lives = 9}
      d = Dog {name = "Rex", age = 3}
  putStrLn (catName c)
  print (dogYears d)
  print (c, d)
  print (case d of Dog {name = n, age = 3} -> n; _ -> "other")
  -- A field no other module has is used as ever.
  print (lives c, Cat.name c, Dog.name d)
