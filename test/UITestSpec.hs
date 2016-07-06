module UITestSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import UI
import Board
import Data.List.Split


main :: IO ()
main = hspec spec

mockInput :: IO String
mockInput = do
  return "1"

spec :: Spec
spec = do
  describe "UI Tests" $ do
    it "Should return formatted string of empty board with numbers for spaces" $ do
      buildBoardString (chunksOf 3 (zip [1..] (concat (newBoard 3)))) `shouldBe` "|_1_|_2_|_3_|\n|_4_|_5_|_6_|\n|_7_|_8_|_9_|\n"

    it "should convert empty row of symbols to string" $ do
      rowToString [(1, empty), (2, empty), (3, empty)] `shouldBe` "|_1_|_2_|_3_|\n"

    it "should convert non-empty row of symbols to string" $ do
      rowToString [(1, empty), (2, x), (3, empty)] `shouldBe` "|_1_|_\ESC[31mX\ESC[0m_|_3_|\n"

    it "should return false on invalid input" $ do
      isValidMoveInput "z" `shouldBe` False

    it "should return true on valid input" $ do
      isValidMoveInput "1" `shouldBe` True

    it "should return false on invalid menu input" $ do
      isValidMenuInput "z" `shouldBe` False
      isValidMenuInput "4" `shouldBe` False

    it "should return true on valid menu input" $ do
      isValidMenuInput "1" `shouldBe` True


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

    it "should be string of number if empty" $ do
      symbolToString (1,empty) `shouldBe` "1"

    it "should be string of symbol if not empty" $ do
      symbolToString (1, x) `shouldBe` "\ESC[31mX\ESC[0m"

    it "should be string of symbol if not empty" $ do
      symbolToString (1, o) `shouldBe` "\ESC[32mO\ESC[0m"

    it "should return the input from the user" $ do
      getUserMoveInput [[empty, empty, x],
                     [x, x, x],
                     [x, empty, x]] mockInput `shouldReturn`  "1"

    it "should return the input from the user menu" $ do
      getUserMenuInput mockInput `shouldReturn`  "1"

    it "Should Return Welcome String" $ do
      welcomeMessage `shouldBe` "\ESC[2J\ESC[HWelcome To Haskell Tic Tac Toe!"

    it "Should Return red encoded symbol in string format" $ do
      setSymbolToRed (1, x) `shouldBe` "\ESC[31mX\ESC[0m"

    it "Should Return red encoded symbol in string format" $ do
      setSymbolToGreen (1, o) `shouldBe` "\ESC[32mO\ESC[0m"

