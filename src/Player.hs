module Player where

import Board

data Player = Player { playerType :: String
                     , playerSymbol :: Symbol
                     } deriving (Show)

currentPlayerType :: (Player, Player) -> String
currentPlayerType players = (playerType (fst players))

currentPlayerSymbol :: (Player, Player) -> Symbol
currentPlayerSymbol players = (playerSymbol (fst players))
