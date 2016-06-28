module Board
  ( Symbol
  , empty
  , x
  , newBoard
  , updateNthElement
  , updateBoard
  ) where

import Data.List.Split

data Symbol = E
            | X
            deriving (Eq, Show)

empty = E
x = X

newBoard :: Int -> [[Symbol]]
newBoard size = (take size(repeat (take size (repeat empty))))

updateNthElement :: Int -> Symbol -> [Symbol] -> [Symbol]
updateNthElement space symbol board = take (space - 1) board ++ [symbol] ++ drop space board

updateBoard :: Int -> Symbol -> [[Symbol]] -> [[Symbol]]
updateBoard space symbol board = chunksOf (length board) (updateNthElement space symbol (concat board))


