module UITestSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import UI
import Board

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "UI Tests" $ do
    it "Should print empty board with numbers for spaces" $ do
      buildBoardString (newBoard 3) `shouldBe` "E E E\nE E E\nE E E\n"

    it "should convert empty row of symbols to string" $ do
      rowToString [empty, empty, empty] `shouldBe` "E E E\n"

    it "should convert non-empty row of symbols to string" $ do
      rowToString [empty, x, empty] `shouldBe` "E X E\n"

    it "should return false on invalid input" $ do
      isValidInput "z" `shouldBe` False

    it "should return true on valid input" $ do
      isValidInput "1" `shouldBe` True

    it "should return true on empty space" $ do
      isValidSpace [[empty, x, empty], [x, x, x], [x, x, x]] "1" `shouldBe` True

    it "should return false on non empty space" $ do
      isValidSpace [[empty, x, empty], [x, x, x], [x, x, x]] "2" `shouldBe` False

    it "should return true on non empty space in correct range" $ do
      isValidMove [[x, empty, x], [x, x, x], [x, x, x]] "2" `shouldBe` True
