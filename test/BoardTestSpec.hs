module BoardTestSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Board

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
