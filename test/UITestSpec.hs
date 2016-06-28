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
