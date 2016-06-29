module UI
      ( buildBoardString
      , rowToString
      , isValidInput
      , get
      , printBoard
      , getUserInput
      , inputPrompt
      , gameOver
      ) where

import Board
import Text.Regex.Posix

buildBoardString :: [[Symbol]] -> String
buildBoardString board = concat (map rowToString board)

rowToString :: [Symbol] -> String
rowToString row = (unwords (map show row)) ++ "\n"

isValidInput :: String -> Bool
isValidInput userInput = userInput =~ "^[1-9]$"

get :: IO String
get = getLine

inputPrompt :: IO String
inputPrompt = do
  putStr "Please choose a space(1-9): "
  getLine

getUserInput :: IO String
getUserInput = do
  input <- inputPrompt
  if isValidInput input
    then return input
    else getUserInput

printBoard :: [[Symbol]] -> IO ()
printBoard board = putStr (buildBoardString board)

gameOver = do
  putStrLn "Game Over!"
