module Game
  ( play
  , makeMove
  ) where

import Board
import UI
import Data.Tuple
import AI
import System.Console.ANSI

makeMove board players = do
  if (fst players) == x
    then do
      input <- getUserInput board
      play (updateBoard (read input) (fst players) board) (swap players)
    else do
      clearFromCursorToScreenBeginning
      putStrLn "Computer Thinking"
      play (updateBoard (minimaxMove board players) (fst players) board) (swap players)

play board players = do
  printBoard board
  if gameIsOver board
    then gameOver
    else makeMove board players

