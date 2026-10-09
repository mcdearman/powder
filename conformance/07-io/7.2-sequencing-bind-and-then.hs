-- Haskell 2010 Report 7.2: sequencing I/O with (>>=) and (>>); effects happen left to right, results are passed on
say :: String -> IO ()
say = putStrLn

produce :: String -> Int -> IO Int
produce label n = putStrLn ("produce " ++ label) >> return n

main :: IO ()
main =
  say "one"
    >> say "two"
    >> produce "a" 1
    >>= \a ->
      produce "b" 2
        >>= \b ->
          print ("a + b", a + b)
            >> (produce "c" 3 >>= print)
            >> (produce "d" 4 >> produce "e" 5 >>= \e -> print ("result of the last action", e))
            >> (return 10 >>= \x -> return (x * 2) >>= \y -> print ("pure chain", x, y :: Int))
            >> (produce "ignored result" 99 >> say "after ignoring")
            >> ((say "left nested" >> say "then") >> say "finally")
            >> (say "right nested" >> (say "then" >> say "finally"))
            >> (produce "f" 6 >>= (\f -> produce "g" (f + 1)) >>= (\g -> print ("threaded", g)))
            >> (produce "h" 7 >>= \h -> produce "i" 8 >>= \i -> print ("both in scope", h, i))
            >> say "done"
