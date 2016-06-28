module Main where

import Board
import UI

main :: IO ()
main = putStr (buildBoardString (newBoard 3))
