-- NumDecimals is not the Report's: without the extension a number with an exponent is a fraction
main :: IO ()
main = print (1e3 :: Int)
