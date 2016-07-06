module UI
      ( buildBoardString
      , rowToString
      , isValidMoveInput
      , isValidMenuInput
      , get
      , printBoard
      , getUserMoveInput
      , getUserMenuInput
      , inputPrompt
      , inputMenuPrompt
      , gameOver
      , getGameOutcomeString
      , isValidSpace
      , isValidMove
      , addIndices
      , symbolToString
      , thinkingMessage
      , clearScreenHome
      , welcomeMessage
      , setSymbolToRed
      , setSymbolToGreen
      ) where

import Board
import Data.List.Split
import Data.List
import Text.Regex.Posix
import System.IO
import System.Console.ANSI

buildBoardString :: [[(Int , Symbol)]] -> String
buildBoardString board = concat (map rowToString board)

rowToString :: [(Int , Symbol)] -> String
rowToString row = "|_" ++ (concat (intersperse "_|_" (map symbolToString row))) ++ "_|\n"

isValidMenuInput :: String -> Bool
isValidMenuInput userInput = userInput =~ "^[1-3]$"

isValidMoveInput :: String -> Bool
isValidMoveInput userInput = userInput =~ "^[1-9]$"

get :: IO String
get = getLine

clearScreenHome :: String
clearScreenHome = "\ESC[2J\ESC[H"

welcomeMessage :: String
welcomeMessage = clearScreenHome ++ "Welcome To Haskell Tic Tac Toe!"

menuPromptMessage :: IO ()
menuPromptMessage = do
  putStrLn "1) Human vs Human"
  putStrLn "2) Human vs Computer"
  putStrLn "3) Computer vs Computer"
  putStr "Please choose a game type(1-3): "

inputMovePromptMessage :: IO ()
inputMovePromptMessage = putStr "Please choose a space(1-9): "

gameOver :: [[Symbol]] -> IO ()
gameOver board = do
  printBoard board
  putStrLn (getGameOutcomeString board)


getGameOutcomeString :: [[Symbol]] -> String
getGameOutcomeString board = do
  if getWinner board == Board.empty
    then "Tie."
    else (show (getWinner board)) ++ " Wins!"

thinkingMessage :: String
thinkingMessage = "Computer is thinking!"

inputPrompt :: IO String
inputPrompt = do
  inputMovePromptMessage
  hFlush stdout
  getLine

inputMenuPrompt :: IO String
inputMenuPrompt = do
  menuPromptMessage
  hFlush stdout
  getLine

getUserMenuInput :: IO String -> IO String
getUserMenuInput inputMenuPrompt = do
  input <- inputMenuPrompt
  if isValidMenuInput input
    then return input
    else getUserMenuInput inputMenuPrompt

getUserMoveInput :: [[Symbol]] -> IO String -> IO String
getUserMoveInput board inputPrompt = do
  input <- inputPrompt
  if isValidMove board input
    then return input
    else getUserMoveInput board inputPrompt

printBoard :: [[Symbol]] -> IO ()
printBoard board = do
  putStr clearScreenHome
  putStr (buildBoardString (addIndices board))

isValidSpace :: [[Symbol]] -> String -> Bool
isValidSpace board space = ((concat board) !! ((read space)-1)) == Board.empty

isValidMove :: [[Symbol]] -> String -> Bool
isValidMove board space = (isValidMoveInput space) && (isValidSpace board space)

addIndices :: [[Symbol]] -> [[(Int , Symbol)]]
addIndices board = chunksOf (length board) (zip [1..] (concat board))

setSymbolToRed :: (Int, Symbol) -> String
setSymbolToRed indexedSymbol = "\x1b[31m" ++ (show (snd indexedSymbol)) ++ "\x1b[0m"

setSymbolToGreen :: (Int, Symbol) -> String
setSymbolToGreen indexedSymbol = "\x1b[32m" ++ (show (snd indexedSymbol)) ++ "\x1b[0m"

symbolToString :: (Int, Symbol) -> String
symbolToString indexedSymbol = do
  if (snd indexedSymbol) == Board.empty
    then show (fst indexedSymbol)
    else do
      if (snd indexedSymbol) == x
        then setSymbolToRed indexedSymbol
        else setSymbolToGreen indexedSymbol
