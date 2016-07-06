module PlayerSpec (spec) where

import Player
import Game
import Board
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

players = ( Player {playerType="human", playerSymbol=x}
          , Player {playerType="computer", playerSymbol=o}
          )

spec :: Spec
spec = do
  describe "Player Tests" $ do
    it "Should return playerType of player" $ do
      let newPlayer = Player {playerType="human", playerSymbol=x}
      playerType newPlayer `shouldBe` "human"

    it "Should return symbol of player" $ do
      let newPlayer = Player {playerType="human", playerSymbol=x}
      playerSymbol newPlayer `shouldBe` x

    it "Should return type of current player" $ do
      currentPlayerType players `shouldBe` "human"

    it "Should return symbol of current player" $ do
      currentPlayerSymbol players `shouldBe` x

