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
      (newBoard 3) `shouldBe` (take 9 (repeat empty))



