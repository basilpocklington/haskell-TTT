module AI
        ( extractOptimalMove
        , getWinnerPoints
        , calculatePoints
        , minimaxMove
        ) where

import Board
import Data.List
import Data.Function
import Data.Tuple


extractOptimalMove ::  (Symbol, Symbol) -> [(Int, Int)] -> Int
extractOptimalMove player scoredMoves = do
  let sortedScoredMoves = sortBy (compare `on` snd) scoredMoves
  if (fst player) == o
    then fst (head sortedScoredMoves)
    else fst (last sortedScoredMoves)

extractOptimalScore ::  Symbol -> [Int] -> Int
extractOptimalScore player scores = do
  if player == o
    then minimum scores
    else maximum scores

getWinnerPoints :: Symbol -> Int -> Int
getWinnerPoints player depth = do
  if player == o
    then depth - 10
    else 10 - depth

calculatePoints :: [[Symbol]] -> (Symbol, Symbol) -> Int -> Int
calculatePoints board players depth = do
  let winner = getWinner board
  if winner == empty
    then 0
    else getWinnerPoints winner depth

score :: [[Symbol]] -> (Symbol, Symbol) -> Int -> Int
score board players depth = do
  if gameIsOver board
    then calculatePoints board players depth
    else do
      let availableMoves = getEmptySpaces board
      let scores = (map (\m -> (score (updateBoard m (fst players) board)) (swap players) (succ depth)) availableMoves)
      extractOptimalScore (fst players) scores

minimaxMove :: [[Symbol]] -> (Symbol, Symbol) -> Int
minimaxMove board players = do
  let availableMoves = getEmptySpaces board
  let scores = (map (\m -> (score (updateBoard m (fst players) board)) (swap players) 1) availableMoves)
  let scoredMoves = zip availableMoves scores
  extractOptimalMove players scoredMoves
