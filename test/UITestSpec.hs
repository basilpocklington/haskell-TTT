module UITestSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import UI
import Board
import Data.List.Split


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "UI Tests" $ do
    it "Should print empty board with numbers for spaces" $ do
      buildBoardString (chunksOf 3 (zip [1..] (concat (newBoard 3)))) `shouldBe` "1 2 3\n4 5 6\n7 8 9\n"

    it "should convert empty row of symbols to string" $ do
      rowToString [(1, empty), (2, empty), (3, empty)] `shouldBe` "1 2 3\n"

    it "should convert non-empty row of symbols to string" $ do
      rowToString [(1, empty), (2, x), (3, empty)] `shouldBe` "1 X 3\n"

    it "should return false on invalid input" $ do
      isValidInput "z" `shouldBe` False

    it "should return true on valid input" $ do
      isValidInput "1" `shouldBe` True

    it "should return true on empty space" $ do
      isValidSpace [[empty, x, empty],
                     [x, x, x],
                     [x, x, x]] "1" `shouldBe` True

    it "should return false on non empty space" $ do
      isValidSpace [[empty, x, empty],
                     [x, x, x],
                     [x, x, x]] "2" `shouldBe` False

    it "should return true on non empty space in correct range" $ do
      isValidMove [[x, empty, x],
                     [x, x, x],
                     [x, x, x]] "2" `shouldBe` True

    it "should return board with indices" $ do
      addIndices  [[x, empty, x],
                     [x, x, x],
                     [x, empty, x]] `shouldBe`
                                  [[(1, x), (2, empty), (3, x)]
                                  , [(4, x), (5, x), (6, x)]
                                  ,[(7, x), (8, empty), (9, x)]]

    it "should string of number if empty" $ do
      symbolToString (1,empty) `shouldBe` "1"

    it "should string of symbol if not empty" $ do
      symbolToString (1, x) `shouldBe` "X"

