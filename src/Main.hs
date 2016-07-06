module Main where

import Game
import Board
import UI
import Control.Concurrent

main :: IO ()
main = do
  clearScreenHome
  welcomeMessage
  threadDelay 1000000
  play (newBoard 3) (x,o)
