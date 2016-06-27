module Board 
  ( Symbol 
  , empty
  , x
  , newBoard
  , updateBoard
  ) where

data Symbol = E
            | X
            deriving (Eq)

empty = E
x = X

newBoard :: Int -> [Symbol] 
newBoard size = take (size * size) (repeat empty) 

updateBoard :: Int -> Symbol -> [Symbol] -> [Symbol]
updateBoard space symbol board = take (space - 1) board ++ [symbol] ++ drop space board

instance Show Symbol where
  show E = show '_'
  show X = show 'x'
