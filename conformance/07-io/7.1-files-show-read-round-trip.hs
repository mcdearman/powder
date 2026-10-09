-- Haskell 2010 Report 7.1: storing values with show/writeFile and recovering them with readFile/read (or readIO)
data Colour = Red | Green | Blue deriving (Eq, Show, Read)

data Item = Item String Int (Maybe Colour) deriving (Eq, Show, Read)

path :: FilePath
path = "/tmp/powder-conformance-7.1-files-show-read-round-trip"

slurp :: FilePath -> IO String
slurp p = do
  s <- readFile p
  length s `seq` return s

roundTrip :: (Show a, Read a) => a -> IO a
roundTrip x = do
  writeFile path (show x)
  s <- slurp path
  readIO s

items :: [Item]
items = [Item "apple" 3 (Just Red), Item "kiwi \"gold\"" (-1) Nothing, Item "" 0 (Just Blue)]

main :: IO ()
main = do
  n <- roundTrip (42 :: Int)
  print ("Int", n)
  xs <- roundTrip [1.5, -2.25, 1.0e-3 :: Double]
  print ("[Double]", xs)
  t <- roundTrip ('x', "a\nb\tc", True, ())
  print ("tuple with escapes", t)
  m <- roundTrip [Just (Left 3), Just (Right "r"), Nothing :: Maybe (Either Int String)]
  print ("Maybe Either", m)
  is <- roundTrip items
  print ("derived Show/Read", is == items)
  mapM_ print is
  big <- roundTrip (2 ^ (200 :: Int) :: Integer)
  print ("Integer", big == 2 ^ (200 :: Int))
  writeFile path (unlines (map show items))
  s <- slurp path
  let back = map read (lines s) :: [Item]
  print ("one value per line", back == items, length back)
  appendFile path (show (Item "extra" 9 (Just Green)) ++ "\n")
  s2 <- slurp path
  print ("after append", last (map read (lines s2) :: [Item]))
