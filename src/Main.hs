module Main where

import Board
import UI

main :: IO ()
main = do
  input <- getUserInput
  putStr (buildBoardString (updateBoard (read input) o (newBoard 3)))
