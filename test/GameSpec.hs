module GameSpec (spec) where

import Game
import Board
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Game Tests" $ do
    it "Should print game over" $ do
      1`shouldBe` 1
