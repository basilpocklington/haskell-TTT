module UI
      (buildBoardString
      , rowToString
      ) where

import Board

buildBoardString :: [[Symbol]] -> String
buildBoardString board = concat (map rowToString board)

rowToString :: [Symbol] -> String
rowToString row = (unwords (map show row)) ++ "\n"
