module AI where

import Board
import Player
import Data.List
import Data.Function
import Data.Tuple
import Control.Parallel.Strategies


extractOptimalMove ::  (Player, Player) -> [(Int, Int)] -> Int
extractOptimalMove player scoredMoves = do
  let sortedScoredMoves = sortBy (compare `on` snd) scoredMoves
  if (currentPlayerSymbol player) == o
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

calculatePoints :: [[Symbol]] -> (Player, Player) -> Int -> Int
calculatePoints board players depth = do
  let winner = getWinner board
  if winner == empty
    then 0
    else getWinnerPoints winner depth

getAllScoresForCurrentBoardState :: [[Symbol]] -> (Player, Player) -> Int -> [Int]
getAllScoresForCurrentBoardState board players depth = do
  let availableMoves = getEmptySpaces board
  (parMap rpar (\m -> (getOptimalScore (updateBoard m (currentPlayerSymbol players) board)) (swap players) (succ depth)) availableMoves)

getOptimalScore :: [[Symbol]] -> (Player, Player) -> Int -> Int
getOptimalScore board players depth = do
  if gameIsOver board
    then calculatePoints board players depth
    else extractOptimalScore (currentPlayerSymbol players) (getAllScoresForCurrentBoardState board players depth)

minimaxMove :: [[Symbol]] -> (Player, Player) -> Int  -> Int
minimaxMove board players depth = do
  let availableMoves = getEmptySpaces board
  let scores = getAllScoresForCurrentBoardState board players depth
  let scoredMoves = zip availableMoves scores
  extractOptimalMove players scoredMoves
