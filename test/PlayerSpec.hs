module PlayerSpec (spec) where

import Player
import Game
import Board
import Test.Hspec

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

    it "Should return a tuple with two new players" $ do
      let newPlayers = createPlayers "human" x "computer" o
      playerSymbol (fst newPlayers) `shouldBe` x
      playerType (fst newPlayers) `shouldBe` "human"
      playerSymbol (snd newPlayers) `shouldBe` o
      playerType (snd newPlayers) `shouldBe` "computer"

    it "Should return a tuple with two humans" $ do
      let newPlayers = humanVsHuman
      playerSymbol (fst newPlayers) `shouldBe` x
      playerType (fst newPlayers) `shouldBe` "human"
      playerSymbol (snd newPlayers) `shouldBe` o
      playerType (snd newPlayers) `shouldBe` "human"

    it "Should return a tuple with human and computer" $ do
      let newPlayers = humanVsComputer
      playerSymbol (fst newPlayers) `shouldBe` x
      playerType (fst newPlayers) `shouldBe` "human"
      playerSymbol (snd newPlayers) `shouldBe` o
      playerType (snd newPlayers) `shouldBe` "computer"

    it "Should return a tuple with two computers" $ do
      let newPlayers = computerVsComputer
      playerSymbol (fst newPlayers) `shouldBe` x
      playerType (fst newPlayers) `shouldBe` "computer"
      playerSymbol (snd newPlayers) `shouldBe` o
      playerType (snd newPlayers) `shouldBe` "computer"

