module Board
  ( Symbol
  , empty
  , x
  , newBoard
  , updateBoard
  ) where

data Symbol = E
            | X
            deriving (Eq, Show)

empty = E
x = X

newBoard :: Int -> [[Symbol]]
newBoard size = (take size(repeat (take size (repeat empty))))

updateBoard :: Int -> Symbol -> [Symbol] -> [Symbol]
updateBoard space symbol board = take (space - 1) board ++ [symbol] ++ drop space board


