module Game where

import Board
import UI
import MenuUI
import Player
import Data.Tuple
import AI
import Control.Concurrent


takeHumanTurn :: [[Symbol]] -> (Player, Player) -> IO ()
takeHumanTurn board players = do
  printBoard board
  input <- getUserMoveInput board inputPrompt
  play (updateBoard (read input) (currentPlayerSymbol players) board) (swap players)

takeComputerTurn :: [[Symbol]] -> (Player, Player) -> IO ()
takeComputerTurn board players = do
  printBoard board
  putStrLn thinkingMessage
  play (updateBoard (minimaxMove board players 0) (currentPlayerSymbol players) board) (swap players)

makeMove :: [[Symbol]] -> (Player, Player) -> IO ()
makeMove board players = do
  if currentPlayerType players == "human"
    then takeHumanTurn board players
    else takeComputerTurn board players

play :: [[Symbol]] -> (Player, Player) -> IO ()
play board players = do
  if gameIsOver board
    then gameOver board
    else makeMove board players

menuSelect :: String -> (Player, Player)
menuSelect choice = do
  if choice == "1"
    then humanVsHuman
    else do
      if choice == "2"
        then humanVsComputer
        else computerVsComputer

start :: IO ()
start = do
  input <- getUserMenuInput inputMenuPrompt
  play (newBoard 3) (menuSelect input)
