Haskell 2010 Report 10.4: literate source, LaTeX style. Program text is
delimited by \begin{code} ... \end{code}; everything else is commentary.

\documentclass{article}
\begin{document}

This text is not Haskell: data = = where; "unterminated

\begin{code}
module Main (main) where

import Data.List (sort)

data Shape = Circle Int | Rect Int Int
  deriving Show
\end{code}

More commentary between code blocks. The layout of the top-level block
continues across blocks.

\begin{code}
area :: Shape -> Int
area (Circle r) = 3 * r * r
area (Rect w h) = w * h

main :: IO ()
main = do
  print ("shapes", [Circle 1, Rect 2 3])
  print ("areas", sort (map area [Rect 2 3, Circle 1]))
\end{code}

\end{document}
