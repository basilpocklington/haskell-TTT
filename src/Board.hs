module Board
  ( Symbol
  , empty
  , x
  , o
  , newBoard
  , updateNthElement
  , updateBoard
  , isFull
  , gameIsOver
  , checkRowWinner
  , containsEmptySpace
  , getDiagonal
  , getAntiDiagonal
  , getColumns
  , getAllCombinations
  , getWinner
  , getEmptySpaces
  ) where

import Data.List.Split
import Data.List

data Symbol = E
            | X
            | O
            deriving (Eq, Show)

empty = E
x = X
o = O

newBoard :: Int -> [[Symbol]]
newBoard size = (take size(repeat (take size (repeat empty))))

updateNthElement :: Int -> Symbol -> [Symbol] -> [Symbol]
updateNthElement space symbol board = take (space - 1) board ++ [symbol] ++ drop space board

updateBoard :: Int -> Symbol -> [[Symbol]] -> [[Symbol]]
updateBoard space symbol board = chunksOf (length board) (updateNthElement space symbol (concat board))

isFull :: [[Symbol]] -> Bool
isFull board = not(empty `elem` (concat board))

gameIsOver :: [[Symbol]] -> Bool
gameIsOver board = not((getWinner board) == empty) || (isFull board)

containsEmptySpace :: [Symbol] -> Bool
containsEmptySpace row = empty `elem` row

isUniform :: [Symbol] -> Bool
isUniform row = (length (group row)) == 1

checkRowWinner :: [Symbol] -> Bool
checkRowWinner row = ((not(containsEmptySpace row)) && (isUniform row))

getDiagonal :: [[Symbol]] -> [Symbol]
getDiagonal board = zipWith (!!) board [0..]

getAntiDiagonal :: [[Symbol]] -> [Symbol]
getAntiDiagonal board = getDiagonal (map reverse board)

getColumns :: [[Symbol]] -> [[Symbol]]
getColumns board = transpose board

getAllCombinations :: [[Symbol]] -> [[Symbol]]
getAllCombinations board = ((board ++ (getColumns  board)) ++ [getDiagonal board]) ++ [getAntiDiagonal board]

getEmptySpaces :: [[Symbol]] -> [Integer]
getEmptySpaces board = map fst (filter ((==Board.empty ).snd) (zip [1..] (concat board)))

getWinner :: [[Symbol]] -> Symbol
getWinner board = do
  let winner = filter checkRowWinner (getAllCombinations board)
  if (length winner) >= 1
    then head (head winner)
    else empty
