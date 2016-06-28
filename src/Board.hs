module Board
  ( Symbol
  , empty
  , x
  , o
  , newBoard
  , updateNthElement
  , updateBoard
  , isFull
  ) where

import Data.List.Split

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


