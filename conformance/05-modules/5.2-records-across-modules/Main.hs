-- Haskell 2010 Report 5.2 / 3.15: field names are exported/imported like other names; construction, selection, update and
-- pattern matching with imported fields, including a field shared by several constructors
module Main (main) where

import Config (Config(..), Mode(Fast, Slow, speed), defaults, Level)
import qualified Config as C

bump :: Config -> Config
bump c@Config { retries = r } = c { retries = r + 1 }

levelOf :: Config -> Level
levelOf = level

main :: IO ()
main = do
  print ("defaults", defaults)
  print ("selection", name defaults, retries defaults, level defaults)
  print ("construction any order", Config { retries = 1, level = 9, name = "x" })
  print ("update one", defaults { name = "renamed" })
  print ("update several", defaults { retries = 0, level = 2 })
  print ("pattern + update", bump defaults, levelOf (bump defaults))
  print ("qualified fields", C.name defaults, defaults { C.retries = 42 })
  print ("shared field across constructors", speed (Fast 10), speed (Slow 1 "why"))
  print ("update of shared field", (Fast 10) { speed = 11 }, (Slow 1 "why") { speed = 2 })
  print ("field of a constructor that was not imported by name", C.reason (Slow 3 "because"))
  print ("fields as functions", map retries [defaults, bump defaults], (name . bump) defaults)
