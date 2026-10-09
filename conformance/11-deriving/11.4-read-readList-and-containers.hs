-- Haskell 2010 Report 11.4: derived Read inside lists (readList: "[A,B , C]"), tuples, Maybe and Either, with free
-- white space and optional parentheses
module Main (main) where

infixr 5 :+:

data E = A | B | C deriving (Eq, Show, Read)

data P = P Int Int deriving (Eq, Show, Read)

data I = Int :+: Int deriving (Eq, Show, Read)

data R = R {f :: Int} deriving (Eq, Show, Read)

rl :: String -> [([E], String)]
rl = readList

main :: IO ()
main = do
  print ("list", read "[A,B]" :: [E], read "[A,B , C]" :: [E], read "[]" :: [E], read "[C]" :: [E])
  print ("list white space", read " [ A , B ] " :: [E], read "[\nA,\nB\n]" :: [E], read " [ ] " :: [E])
  print ("list parentheses", read "([A,B])" :: [E], read "[(A),((B))]" :: [E], read " ( [ ( A ) ] ) " :: [E])
  print ("the value itself", read "  ( A )  " :: E, read "((B))" :: E)
  print ("nested lists", read "[[A],[],[B,C]]" :: [[E]], read "[[[A]]]" :: [[[E]]])
  print ("readList", rl "[A,B] rest", rl "[]", rl "[A,B", rl "A,B]", rl "[A B]", rl "[A,,B]")
  print ("readList more failures", rl "[A,]", rl "[,A]", rl "[A;B]", rl "", rl "[a]")
  print ("readList = reads at list type", rl "[C,A] x" == reads "[C,A] x")
  print ("list of prefix", read "[P 1 2,P (-3) 4]" :: [P], read "[ P 1 2 , (P 3 4) ]" :: [P])
  print ("list of infix", read "[1 :+: 2,(-3) :+: 4]" :: [I], read "[(1 :+: 2)]" :: [I])
  print ("list of records", read "[R {f = 1},R {f = -2}]" :: [R])
  print ("tuples", read "(A,B)" :: (E, E), read "( A , P 1 2 , 3 :+: 4 , R {f = 5} )" :: (E, P, I, R))
  print ("tuples of lists", read "([A],[B,C])" :: ([E], [E]), read "[(A,P 0 0)]" :: [(E, P)])
  print ("Maybe", read "Just A" :: Maybe E, read "Just (P 1 2)" :: Maybe P, read "Just (1 :+: 2)" :: Maybe I, read "Just (R {f = 1})" :: Maybe R)
  print ("Maybe nested", read "Just [A,B]" :: Maybe [E], read "Just (A,B)" :: Maybe (E, E), read "Just (Just C)" :: Maybe (Maybe E), read "[Just A,Nothing]" :: [Maybe E])
  print ("Either", read "Left A" :: Either E P, read "Right (P 1 2)" :: Either E P, read "[Left (P 1 2),Right [A,B]]" :: [Either P [E]])
  print ("deep", read "Just (Left (Just [A,B]))" :: Maybe (Either (Maybe [E]) Int), read "[[(Just (P 1 2),[1 :+: 2])]]" :: [[(Maybe P, [I])]])
  print ("unit and derived", read "((),A,[()],[((),B)])" :: ((), E, [()], [((), E)]))
  print ("failures in containers", null (reads "[A,D]" :: [([E], String)]), null (reads "(A,)" :: [((E, E), String)]), null (reads "Just P 1 2" :: [(Maybe P, String)]))
  print ("read . show lists", read (show [A, B, C]) == [A, B, C], read (show [[P 1 2], []]) == [[P 1 2], []], read (show ([] :: [I])) == ([] :: [I]))
  print ("read . show mixed", read (show (A, [P (-1) 2], Just ((-3) :+: 4), [R (-5)])) == (A, [P (-1) 2], Just ((-3) :+: 4), [R (-5)]))
