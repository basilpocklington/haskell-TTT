module UI
      ( buildBoardString
      , rowToString
      , isValidInput
      , get
      , printBoard
      , getUserInput
      , inputPrompt
      , gameOver
      , isValidSpace
      , isValidMove
      ) where

import Board
import Text.Regex.Posix
import System.Console.ANSI

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

getUserInput :: [[Symbol]] -> IO String
getUserInput board = do
  input <- inputPrompt
  if isValidMove board input
    then return input
    else getUserInput board

printBoard :: [[Symbol]] -> IO ()
printBoard board = do
  clearFromCursorToScreenBeginning
  putStr (buildBoardString board)

gameOver = do
  putStrLn "Game Over!"

isValidSpace :: [[Symbol]] -> String -> Bool
isValidSpace board space = ((concat board) !! ((read space)-1)) == Board.empty

isValidMove :: [[Symbol]] -> String -> Bool
isValidMove board space = (isValidInput space) && (isValidSpace board space)
