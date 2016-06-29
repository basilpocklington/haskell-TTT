module Main where

import Game
import Board

main :: IO ()
main = play (newBoard 3) (x,o)
