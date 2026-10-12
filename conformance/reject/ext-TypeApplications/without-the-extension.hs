-- TypeApplications is not the Report's: without the extension this is refused
main :: IO ()
main = print (read @Int "1")
