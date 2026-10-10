-- Haskell 2010 Report 8.3, 8.4.2: an exported function's arguments and result are foreign types; a list is
-- not one
foreign export ccall "powder_conformance_total" total :: [Int] -> Int

total :: [Int] -> Int
total = sum

main :: IO ()
main = return ()
