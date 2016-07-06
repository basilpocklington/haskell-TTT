module Player where

import Board

data Player = Player { playerType :: String
                     , playerSymbol :: Symbol
                     } deriving (Show)

currentPlayerType :: (Player, Player) -> String
currentPlayerType players = (playerType (fst players))

currentPlayerSymbol :: (Player, Player) -> Symbol
currentPlayerSymbol players = (playerSymbol (fst players))

createPlayers :: String -> Symbol -> String -> Symbol -> (Player, Player)
createPlayers playerType1 playerSymbol1 playerType2 playerSymbol2 =
  ( Player {playerType=playerType1, playerSymbol=playerSymbol1}
  , Player {playerType=playerType2, playerSymbol=playerSymbol2}
  )

humanVsHuman :: (Player, Player)
humanVsHuman = createPlayers "human" x "human" o

humanVsComputer :: (Player, Player)
humanVsComputer = createPlayers "human" x "computer" o

computerVsComputer :: (Player, Player)
computerVsComputer = createPlayers "computer" x "computer" o
