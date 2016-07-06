module Game
  ( play
  , makeMove
  , takeHumanTurn
  , takeComputerTurn
  , start
  ) where

import Board
import UI
import Data.Tuple
import AI
import Control.Concurrent


takeHumanTurn :: [[Symbol]] -> (Symbol, Symbol) -> IO ()
takeHumanTurn board players = do
  printBoard board
  input <- getUserInput board inputPrompt
  play (updateBoard (read input) (fst players) board) (swap players)

takeComputerTurn :: [[Symbol]] -> (Symbol, Symbol) -> IO ()
takeComputerTurn board players = do
  printBoard board
  putStrLn thinkingMessage
  play (updateBoard (minimaxMove board players 0) (fst players) board) (swap players)

makeMove :: [[Symbol]] -> (Symbol, Symbol) -> IO ()
makeMove board players = do
  if (fst players) == x
    then takeHumanTurn board players
    else takeComputerTurn board players

play :: [[Symbol]] -> (Symbol, Symbol) -> IO ()
play board players = do
  if gameIsOver board
    then gameOver board
    else makeMove board players

start :: IO ()
start = do
  putStrLn welcomeMessage
  threadDelay 1000000
  play (newBoard 3) (x,o)
