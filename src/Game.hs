module Game
  ( play
  , makeMove
  , takeHumanTurn
  , takeComputerTurn
  ) where

import Board
import UI
import Data.Tuple
import AI

takeHumanTurn :: [[Symbol]] -> (Symbol, Symbol) -> IO ()
takeHumanTurn board players = do
  printBoard board
  input <- getUserInput board
  play (updateBoard (read input) (fst players) board) (swap players)

takeComputerTurn :: [[Symbol]] -> (Symbol, Symbol) -> IO ()
takeComputerTurn board players = do
  printBoard board
  thinkingMessage
  play (updateBoard (minimaxMove board players) (fst players) board) (swap players)

makeMove :: [[Symbol]] -> (Symbol, Symbol) -> IO ()
makeMove board players = do
  if (fst players) == x
    then takeHumanTurn board players
    else takeComputerTurn board players

play :: [[Symbol]] -> (Symbol, Symbol) -> IO ()
play board players = do
  if gameIsOver board
    then do
      printBoard board
      gameOver
    else makeMove board players

