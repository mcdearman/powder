-- Haskell 2010 Report 11.4: derived Read for record syntax: only the record form is accepted, with the fields in the
-- order of the declaration; white space is free and the whole value may be parenthesised
module Main (main) where

data C = C {f1 :: Int, f2 :: String} deriving (Eq, Show, Read)

data Single = Single {only :: Double} deriving (Eq, Show, Read)

data Shape = Circle {radius :: Int} | Rect {width :: Int, height :: Int} | Point | Poly [Int] deriving (Eq, Show, Read)

data Outer = Outer {inner :: C, flag :: Bool, opt :: Maybe C} deriving (Eq, Show, Read)

data Op = Op {(%%) :: Int, plain :: Char} deriving (Eq, Show, Read)

rc :: String -> [(C, String)]
rc = reads

main :: IO ()
main = do
  print ("canonical", read "C {f1 = 1, f2 = \"x\"}" :: C)
  print ("no spaces", read "C{f1=1,f2=\"x\"}" :: C)
  print ("many spaces", read "  C  {  f1  =  1  ,  f2  =  \"x\"  }  " :: C)
  print ("newlines and tabs", read "C {\n\tf1 = 1,\n\tf2 = \"x\"\n}" :: C)
  print ("parenthesised", read "(C {f1 = 1, f2 = \"x\"})" :: C, read " ( ( C {f1 = 1, f2 = \"x\"} ) ) " :: C)
  print ("negative field", read "C {f1 = -1, f2 = \"\"}" :: C, read "C {f1 = (-1), f2 = \"\"}" :: C)
  print ("parenthesised field values", read "C {f1 = (1), f2 = (\"x\")}" :: C)
  print ("string field with special characters", read "C {f1 = 0, f2 = \"a, b = {c}\"}" :: C)
  print ("remaining input", rc "C {f1 = 1, f2 = \"x\"} rest", rc "C {f1 = 1, f2 = \"x\"}}")
  print ("wrong field order", rc "C {f2 = \"x\", f1 = 1}")
  print ("missing field", rc "C {f1 = 1}", rc "C {f2 = \"x\"}", rc "C {}")
  print ("extra or repeated field", rc "C {f1 = 1, f2 = \"x\", f3 = 0}", rc "C {f1 = 1, f1 = 1, f2 = \"x\"}")
  print ("positional form not accepted", rc "C 1 \"x\"")
  print ("wrong field name or type", rc "C {g1 = 1, f2 = \"x\"}", rc "C {f1 = \"x\", f2 = 1}", rc "C {f1 = 1, f2 = 'x'}")
  print ("punctuation errors", rc "C {f1 = 1; f2 = \"x\"}", rc "C {f1 = 1, f2 = \"x\"", rc "C {f1 : 1, f2 = \"x\"}", rc "C f1 = 1, f2 = \"x\"")
  print ("single field", read "Single {only = 1.5}" :: Single, read "Single {only = -2}" :: Single, read "(Single{only=0.25})" :: Single)
  print ("mixed constructors", read "Circle {radius = 3}" :: Shape, read "Rect {width = 1, height = 2}" :: Shape, read "Point" :: Shape, read "Poly [1,2]" :: Shape)
  print ("mixed constructors failures", null (reads "Circle 3" :: [(Shape, String)]), null (reads "Rect {height = 2, width = 1}" :: [(Shape, String)]), null (reads "Poly {}" :: [(Shape, String)]))
  print ("nested", read "Outer {inner = C {f1 = 1, f2 = \"x\"}, flag = True, opt = Nothing}" :: Outer)
  print ("nested Just", read "Outer {inner = C {f1 = -1, f2 = \"x\"}, flag = False, opt = Just (C {f1 = 2, f2 = \"y\"})}" :: Outer)
  print ("operator field", read "Op {(%%) = 1, plain = 'c'}" :: Op, read "Op { ( %% ) = -2 , plain = 'd' }" :: Op)
  print ("in structures", read "[C {f1 = 1, f2 = \"a\"},C {f1 = 2, f2 = \"b\"}]" :: [C], read "(C {f1 = 1, f2 = \"a\"},Point)" :: (C, Shape))
  print ("in Maybe, parenthesised", read "Just (C {f1 = 1, f2 = \"x\"})" :: Maybe C, read "Just (Circle {radius = 1})" :: Maybe Shape)
  print ("read . show", all (\c -> read (show c) == c) [C 1 "x", C (-1) "", C minBound "q\"q"], all (\s -> read (show s) == s) [Circle 1, Rect (-1) 2, Point, Poly [-1]])
  print ("read . show nested", read (show (Outer (C (-1) "x") True (Just (C 2 "y")))) == Outer (C (-1) "x") True (Just (C 2 "y")), read (show (Just (Op (-5) 'z'))) == Just (Op (-5) 'z'))
