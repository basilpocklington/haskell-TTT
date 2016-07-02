module AI
        ( extractOptimalMove
        , getWinnerPoints
        , calculatePoints
        , minimaxMove
        ,getAllScoresForCurrentBoardState
        ) where

import Board
import Data.List
import Data.Function
import Data.Tuple
import Control.Parallel.Strategies


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

getAllScoresForCurrentBoardState :: [[Symbol]] -> (Symbol, Symbol) -> Int -> [Int]
getAllScoresForCurrentBoardState board players depth = do
  let availableMoves = getEmptySpaces board
  (parMap rpar (\m -> (getOptimalScore (updateBoard m (fst players) board)) (swap players) (succ depth)) availableMoves)

getOptimalScore :: [[Symbol]] -> (Symbol, Symbol) -> Int -> Int
getOptimalScore board players depth = do
  if gameIsOver board
    then calculatePoints board players depth
    else extractOptimalScore (fst players) (getAllScoresForCurrentBoardState board players depth)

minimaxMove :: [[Symbol]] -> (Symbol, Symbol) -> Int  -> Int
minimaxMove board players depth = do
  let availableMoves = getEmptySpaces board
  let scores = getAllScoresForCurrentBoardState board players depth
  let scoredMoves = zip availableMoves scores
  extractOptimalMove players scoredMoves
