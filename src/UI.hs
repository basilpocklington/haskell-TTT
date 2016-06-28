module UI
      (buildBoardString
      , rowToString
      , isValidInput
      , getUserInput
      ) where

import Board
import Text.Regex.Posix

buildBoardString :: [[Symbol]] -> String
buildBoardString board = concat (map rowToString board)

rowToString :: [Symbol] -> String
rowToString row = (unwords (map show row)) ++ "\n"

isValidInput :: String -> Bool
isValidInput userInput = userInput =~ "^[1-9]$"

getUserInput :: IO String
getUserInput = getLine
