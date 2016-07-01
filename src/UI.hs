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
      , addIndices
      , symbolToString
      ) where

import Board
import Data.List.Split
import Text.Regex.Posix
import System.Console.ANSI

buildBoardString :: [[(Integer, Symbol)]] -> String
buildBoardString board = concat (map rowToString board)

rowToString :: [(Integer, Symbol)] -> String
rowToString row = (unwords (map symbolToString row)) ++ "\n"

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
  putStr (buildBoardString (addIndices board))

gameOver = do
  putStrLn "Game Over!"

isValidSpace :: [[Symbol]] -> String -> Bool
isValidSpace board space = ((concat board) !! ((read space)-1)) == Board.empty

isValidMove :: [[Symbol]] -> String -> Bool
isValidMove board space = (isValidInput space) && (isValidSpace board space)

addIndices :: [[Symbol]] -> [[(Integer, Symbol)]]
addIndices board = chunksOf (length board) (zip [1..] (concat board))

symbolToString :: (Integer, Symbol) -> String
symbolToString indexedSymbol = do
  if (snd indexedSymbol) == Board.empty
    then show (fst indexedSymbol)
    else show (snd indexedSymbol)
