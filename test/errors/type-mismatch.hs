f :: Bool -> Char
f c = if c then 'a' else c

main :: IO ()
main = print (f True)
