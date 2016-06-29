module Game
  ( play
  , makeMove
  ) where

import Board
import UI
import Data.Tuple

makeMove board players = do
  input <- getUserInput board
  play (updateBoard (read input) (fst players) board) (swap players)

play board players = do
  printBoard board
  if gameIsOver board
    then gameOver
    else makeMove board players

