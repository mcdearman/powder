-- Haskell 2010 Report 6.1.8: Ordering (LT, EQ, GT) and its Eq, Ord, Bounded, Enum, Read, Show instances
invert :: Ordering -> Ordering
invert LT = GT
invert EQ = EQ
invert GT = LT

thenCmp :: Ordering -> Ordering -> Ordering
thenCmp EQ o = o
thenCmp o _ = o

main :: IO ()
main = do
  print ("constructors", LT, EQ, GT)
  print ("from compare", compare (1 :: Int) 2, compare 'b' 'b', compare "b" "a")
  print ("eq", LT == LT, LT /= EQ, GT == EQ)
  print ("ord", LT < EQ, EQ < GT, LT < GT, compare GT LT, compare EQ EQ, max LT GT)
  print ("min/max", minimum [GT, EQ, LT], maximum [EQ, LT, GT, LT])
  print ("bounded", minBound :: Ordering, maxBound :: Ordering)
  print ("enum all", [LT ..], [minBound .. maxBound :: Ordering])
  print ("fromEnum", map fromEnum [LT, EQ, GT])
  print ("toEnum", map toEnum [0, 1, 2] :: [Ordering])
  print ("succ/pred", succ LT, succ EQ, pred GT, pred EQ)
  print ("enumFromThen", [LT, GT ..], [GT, EQ ..], [GT, LT ..], take 2 [EQ, EQ .. EQ])
  print ("enumFromTo", [EQ .. GT], [GT .. LT], [LT .. LT])
  print ("show", show LT, show [EQ, GT], showsPrec 11 GT "", show (Just LT))
  print ("read", read "LT" :: Ordering, read " GT " :: Ordering, read "(EQ)" :: Ordering, read "[LT,GT]" :: [Ordering])
  print ("reads", reads "LTX" :: [(Ordering, String)], reads "GT rest" :: [(Ordering, String)])
  print ("invert", map invert [LT, EQ, GT])
  print ("lexicographic", thenCmp (compare 'a' 'a') (compare (2 :: Int) 1), thenCmp LT GT)
  print ("case on compare", case compare (3 :: Int) 3 of { LT -> "less"; EQ -> "equal"; GT -> "greater" })
