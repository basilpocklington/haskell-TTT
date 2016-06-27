module Board 
  ( Symbol 
  , empty
  , newBoard
  ) where

data Symbol = E deriving (Eq)

empty = E

newBoard :: Int -> [Symbol] 
newBoard size = take (size * size) (repeat empty) 

instance Show Symbol where
  show E = show '_'
