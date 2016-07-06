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
      , thinkingMessage
      , clearScreen
      ) where

import Board
import Data.List.Split
import Text.Regex.Posix
import System.IO

buildBoardString :: [[(Int , Symbol)]] -> String
buildBoardString board = concat (map rowToString board)

rowToString :: [(Int , Symbol)] -> String
rowToString row = (unwords (map symbolToString row)) ++ "\n"

isValidInput :: String -> Bool
isValidInput userInput = userInput =~ "^[1-9]$"

get :: IO String
get = getLine

clearScreen :: IO ()
clearScreen = putStrLn "\ESC[2J\ESC[H"

inputPromptMessage :: IO ()
inputPromptMessage = putStr "Please choose a space(1-9): "

gameOver :: [[Symbol]] -> IO ()
gameOver board = do
  printBoard board
  putStrLn "Game Over!"

thinkingMessage :: IO ()
thinkingMessage = do
  putStrLn "Computer is thinking!"

inputPrompt :: IO String
inputPrompt = do
  inputPromptMessage
  hFlush stdout
  getLine

getUserInput :: [[Symbol]] -> IO String -> IO String
getUserInput board inputPrompt = do
  input <- inputPrompt
  if isValidMove board input
    then return input
    else getUserInput board inputPrompt

printBoard :: [[Symbol]] -> IO ()
printBoard board = do
  clearScreen
  putStr (buildBoardString (addIndices board))

isValidSpace :: [[Symbol]] -> String -> Bool
isValidSpace board space = ((concat board) !! ((read space)-1)) == Board.empty

isValidMove :: [[Symbol]] -> String -> Bool
isValidMove board space = (isValidInput space) && (isValidSpace board space)

addIndices :: [[Symbol]] -> [[(Int , Symbol)]]
addIndices board = chunksOf (length board) (zip [1..] (concat board))

symbolToString :: (Int , Symbol) -> String
symbolToString indexedSymbol = do
  if (snd indexedSymbol) == Board.empty
    then show (fst indexedSymbol)
    else show (snd indexedSymbol)
