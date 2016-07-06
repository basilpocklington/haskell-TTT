module Player where

import Board

data Player = Player { playerType :: String
                     , playerSymbol :: Symbol
                     } deriving (Show)
