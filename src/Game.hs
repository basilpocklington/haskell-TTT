module Game
  ( play
  , makeMove
  , takeHumanTurn
  , takeComputerTurn
  , start
  ) where

import Board
import UI
import Player
import Data.Tuple
import AI
import Control.Concurrent


takeHumanTurn :: [[Symbol]] -> (Player, Player) -> IO ()
takeHumanTurn board players = do
  printBoard board
  input <- getUserInput board inputPrompt
  play (updateBoard (read input) (playerSymbol (fst players)) board) (swap players)

takeComputerTurn :: [[Symbol]] -> (Player, Player) -> IO ()
takeComputerTurn board players = do
  printBoard board
  putStrLn thinkingMessage
  play (updateBoard (minimaxMove board players 0) (playerSymbol (fst players)) board) (swap players)

makeMove :: [[Symbol]] -> (Player, Player) -> IO ()
makeMove board players = do
  if (playerType (fst players)) == "human"
    then takeHumanTurn board players
    else takeComputerTurn board players

play :: [[Symbol]] -> (Player, Player) -> IO ()
play board players = do
  if gameIsOver board
    then gameOver board
    else makeMove board players

start :: IO ()
start = do
  putStrLn welcomeMessage
  threadDelay 1000000
  play (newBoard 3) (Player {playerType="human", playerSymbol=x},Player {playerType="computer", playerSymbol=o})
