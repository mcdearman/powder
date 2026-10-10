-- | C for the functions a program makes C's to call.
--
-- A function the program exports is made when the program starts, so it
-- has an address then and no name a C library could be linked against.
-- The C here gives it one: for each export, a function of that name and
-- type which calls through a place, @powder_export_@ and the name, that
-- the machine fills with the address when it starts. Compiled into a
-- shared library and loaded before the libraries that call the exports,
-- by naming it first in @POWDER_FOREIGN_LIBS@, it is what those link
-- with.
module Powder.Stubs (stubs) where

import Data.List (intercalate)
import Powder.Core (Export (..))

-- | The C source for some exports.
stubs :: [Export] -> String
stubs exports =
  unlines
    ( [ "/* Written by Powder: the functions a program makes C's to call.",
        " * Compile it into a shared library, and name that first in",
        " * POWDER_FOREIGN_LIBS. */",
        "#include <stdint.h>"
      ]
        ++ concatMap stub exports
    )
  where
    stub (Export name shape result _) =
      [ "",
        "void *powder_export_" ++ name ++ " = 0;",
        "",
        typeOf result ++ " " ++ name ++ "(" ++ parameters ++ ") {",
        "  " ++ (if result == 'V' then "" else "return ") ++ "((" ++ typeOf result ++ " (*)(" ++ types ++ "))powder_export_" ++ name ++ ")(" ++ intercalate ", " names ++ ");",
        "}"
      ]
      where
        names = ["a" ++ show i | i <- [0 .. length shape - 1]]
        types = if null shape then "void" else intercalate ", " (map typeOf shape)
        parameters = if null shape then "void" else intercalate ", " (zipWith (\kind x -> typeOf kind ++ " " ++ x) shape names)
    typeOf :: Char -> String
    typeOf = \case
      'c' -> "int8_t"
      'C' -> "uint8_t"
      's' -> "int16_t"
      'S' -> "uint16_t"
      'I' -> "int32_t"
      'U' -> "uint32_t"
      'L' -> "int64_t"
      'Q' -> "uint64_t"
      'F' -> "float"
      'D' -> "double"
      'V' -> "void"
      _ -> "void *"
