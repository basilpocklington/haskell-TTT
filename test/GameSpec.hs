module GameSpec (spec) where

import Game
import Board
import Player
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Game Tests" $ do
    it "Should return a tuple with two humans" $ do
      let newPlayers = menuSelect "1"
      playerSymbol (fst newPlayers) `shouldBe` x
      playerType (fst newPlayers) `shouldBe` "human"
      playerSymbol (snd newPlayers) `shouldBe` o
      playerType (snd newPlayers) `shouldBe` "human"

    it "Should return a tuple with human and computer" $ do
      let newPlayers = menuSelect "2"
      playerSymbol (fst newPlayers) `shouldBe` x
      playerType (fst newPlayers) `shouldBe` "human"
      playerSymbol (snd newPlayers) `shouldBe` o
      playerType (snd newPlayers) `shouldBe` "computer"

    it "Should return a tuple with two computers" $ do
      let newPlayers = menuSelect "3"
      playerSymbol (fst newPlayers) `shouldBe` x
      playerType (fst newPlayers) `shouldBe` "computer"
      playerSymbol (snd newPlayers) `shouldBe` o
      playerType (snd newPlayers) `shouldBe` "computer"
