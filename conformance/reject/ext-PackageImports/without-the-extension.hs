-- PackageImports is not the Report's: without the extension this is refused
import "base" Data.Char (toUpper)

main :: IO ()
main = putStrLn (map toUpper "x")
