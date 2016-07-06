module Player where

import Board
import UI
import Data.Tuple
import AI
import Control.Concurrent

data Player = Player { playerType :: String
                     , playerSymbol :: Symbol
                     } deriving (Show)
