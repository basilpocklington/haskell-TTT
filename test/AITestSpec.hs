module AITestSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import AI
import Board
import Player

main :: IO ()
main = hspec spec

humanPlayer = Player {playerType="human", playerSymbol=x}
computerPlayer = Player {playerType="computer", playerSymbol=o}

spec :: Spec
spec = do
  describe "AI Tests" $ do
    it "should return optimal max move for o" $ do
      extractOptimalMove (computerPlayer,humanPlayer) [(1,2),(2,3),(3,6),(4,1),(5,2)] `shouldBe` 4

    it "should return optimal max move for x" $ do
      extractOptimalMove (humanPlayer, computerPlayer) [(1,2),(2,3),(3,6),(4,1),(5,2)] `shouldBe` 3

    it "should determine points for o winner based on depth" $ do
      getWinnerPoints o 3 `shouldBe` -7

    it "should determine points for x winner based on depth" $ do
      getWinnerPoints x 3 `shouldBe` 7

    it "should calculate points for x win" $ do
      calculatePoints [[x, o, x],
                       [o, x, o],
                       [x, o, x]] (humanPlayer, computerPlayer) 3 `shouldBe` 7

    it "should calculate points for tie with full board" $ do
      calculatePoints [[o, o, x],
                       [x, x, o],
                       [o, o, x]] (humanPlayer, computerPlayer) 3 `shouldBe` 0

    it "should calculate points for o win" $ do
      calculatePoints [[o, o, x],
                       [x, x, o],
                       [o, o, o]] (humanPlayer, computerPlayer) 3 `shouldBe` -7

    it "should calculate points for empty" $ do
      calculatePoints (newBoard 3) (humanPlayer, computerPlayer) 3 `shouldBe` 0

  describe "minimaxqMove" $ do
    it "1 returns blocking move if no win is possible X on top" $ do
      minimaxMove [[x, x, empty],
                    [empty, empty, empty],
                    [empty, empty, o]] (computerPlayer, humanPlayer) 0`shouldBe` 3

    it "2 returns blocking move if no win is possible O on top" $ do
      minimaxMove [[o, empty, empty],
                    [empty, empty, empty],
                    [empty, x, x]] (computerPlayer, humanPlayer) 0`shouldBe` 7

    it "3 returns win rather than block when possible X on top" $ do
      minimaxMove [[x, x, empty],
                    [empty, empty, empty],
                    [empty, o, o]] (computerPlayer, humanPlayer) 0`shouldBe` 7

    it "4 returns win rather than block when possible O on top" $ do
      minimaxMove [[o, empty, o],
                    [empty, empty, empty],
                    [x, empty, x]] (computerPlayer, humanPlayer) 0`shouldBe` 2

    it "5 returns blocking move if no win is possible" $ do
      minimaxMove [[x, o, o],
                    [empty, x, empty],
                    [empty, empty, empty]] (computerPlayer, humanPlayer) 0`shouldBe` 9

    it "6 returns blocking move if no win is possible" $ do
      minimaxMove [[o, o, x],
                    [empty, x, empty],
                    [empty, empty, empty]] (computerPlayer, humanPlayer) 0`shouldBe` 7

    it "7 returns blocking move if no win is possible" $ do
      minimaxMove [[o, empty, x],
                    [empty, x, empty],
                    [empty, empty, empty]] (computerPlayer, humanPlayer) 0`shouldBe` 7

    it "8 returns blocking move if no win is possible" $ do
      minimaxMove [[empty, empty, empty],
                    [empty, x, o],
                    [empty, empty, x]] (computerPlayer, humanPlayer) 0`shouldBe` 1

    it "9 returns blocking move" $ do
      minimaxMove [[o, o, empty],
                    [empty, empty, empty],
                    [empty, empty, x]] (humanPlayer, computerPlayer) 0`shouldBe` 3

    it "10 returns blocking move" $ do
      minimaxMove [[x, empty, empty],
                    [empty, empty, empty],
                    [empty, o, o]] (humanPlayer, computerPlayer) 0`shouldBe` 7

    it "11 returns winning move" $ do
      minimaxMove [[o, o, empty],
                    [empty, empty, empty],
                    [empty, x, x]] (humanPlayer, computerPlayer) 0`shouldBe` 7

    it "12 returns winning move" $ do
      minimaxMove [[x, empty, x],
                    [empty, empty, empty],
                    [o, empty, o]] (humanPlayer, computerPlayer) 0`shouldBe` 2

    it "13 returns blocking move" $ do
      minimaxMove [[o, x, x],
                    [empty, o, empty],
                    [empty, empty, empty]] (humanPlayer, computerPlayer) 0`shouldBe` 9

    it "14 returns blocking move" $ do
      minimaxMove [[x, x, o],
                    [empty, o, empty],
                    [empty, empty, empty]] (humanPlayer, computerPlayer) 0`shouldBe` 7

    it "15 returns blocking move" $ do
      minimaxMove [[x, empty, o],
                    [empty, o, empty],
                    [empty, empty, empty]] (humanPlayer, computerPlayer) 0`shouldBe` 7

    it "16 returns blocking move" $ do
      minimaxMove [[empty, empty, empty],
                    [empty, o, x],
                    [empty, empty, o]] (humanPlayer, computerPlayer) 0`shouldBe` 1
