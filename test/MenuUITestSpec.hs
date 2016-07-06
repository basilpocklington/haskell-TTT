module MenuUITestSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import MenuUI
import Board
import Data.List.Split


main :: IO ()
main = hspec spec

mockInput :: IO String
mockInput = do
  return "1"

spec :: Spec
spec = do
  describe "Menu UI Tests" $ do

    it "should return the input from the user menu" $ do
      getUserMenuInput mockInput `shouldReturn`  "1"

    it "Should Return Welcome String" $ do
      welcomeMessage `shouldBe` "\ESC[2J\ESC[HWelcome To Haskell Tic Tac Toe!"

    it "should return false on invalid menu input" $ do
      isValidMenuInput "g" `shouldBe` False
      isValidMenuInput "4" `shouldBe` False

    it "should return true on valid menu input" $ do
      isValidMenuInput "1" `shouldBe` True
