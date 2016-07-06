module BoardTestSpec (spec) where

import Board
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Board Tests" $ do
    it "should return new 9 space empty board" $ do
      (newBoard 3) `shouldBe` (take 3(repeat (take 3 (repeat empty))))

    it "should update nth element in list" $ do
      (updateNthElement 1 x (take 3 (repeat empty))) `shouldBe` x:(take 2 (repeat empty))

    it "should update board when symbol is placed" $ do
      (updateBoard 5 x (newBoard 3)) `shouldBe` [[empty, empty, empty], [empty, x, empty], [empty, empty, empty]]

    it "should return false if board is not full" $ do
      isFull (newBoard 3) `shouldBe` False

    it "should return true if board is full" $ do
      isFull [[x, x, x], [x, x, x], [x, x, x]] `shouldBe` True

    it "should return true for uniform non empty data" $ do
      checkRowWinner [x, x, x] `shouldBe` True

    it "should return false for non uniform data" $ do
      checkRowWinner [x, empty, x] `shouldBe` False

    it "should return false for non uniform data" $ do
      checkRowWinner [x, o, x] `shouldBe` False

    it "should return false if no empty spaces" $ do
      containsEmptySpace [x, o, x] `shouldBe` False

    it "should return true if contains empty spaces" $ do
      containsEmptySpace [x, empty, x] `shouldBe` True

    it "should return diagonal" $ do
      getDiagonal [[x, o, o], [o, x, o], [o, o, x]] `shouldBe` [x, x, x]

    it "should return anti-diagonal" $ do
      getAntiDiagonal [[o, o, x], [o, x, o], [x, o, o]] `shouldBe` [x, x, x]

    it "should return columns" $ do
      getColumns [[x, x, x], [o, o, o], [x, x, x]] `shouldBe` [[x,o,x],[x,o,x],[x,o,x]]

    it "should return all winnable combinations" $ do
      getAllCombinations [[x, o, x], [o, x, o], [o, x, x]] `shouldBe` [[x, o, x]
                                                                      ,[o, x, o]
                                                                      ,[o, x, x]
                                                                      ,[x, o, o]
                                                                      ,[o, x, x]
                                                                      ,[x, o, x]
                                                                      ,[x, x, x]
                                                                      ,[x, x, o]]

    it "should return x for x winner" $ do
      getWinner [[x, x, x], [o, empty, o], [empty, empty, empty]] `shouldBe` x

    it "should return empty for no winner on empty board" $ do
      getWinner (newBoard 3) `shouldBe` empty

    it "should return empty for no winner on full tie board" $ do
      getWinner [[o, o, x], [x, x, o], [o, o, x]] `shouldBe` empty

    it "should return x for winner on full board" $ do
      getWinner [[x, o, x], [o, x, o], [x, o, x]] `shouldBe` x

    it "should return true for x winner" $ do
      gameIsOver [[x, x, x], [o, empty, o], [empty, empty, empty]] `shouldBe` True

    it "should return false for no winner" $ do
      gameIsOver [[x, empty, x], [o, empty, o], [empty, empty, empty]] `shouldBe` False

    it "should return false for empty board" $ do
      gameIsOver (newBoard 3) `shouldBe` False

    it "should return a list of empty spaces" $ do
      getEmptySpaces [[x,o,x],[o,empty,o],[o,empty,empty]] `shouldBe` [5, 8, 9]

