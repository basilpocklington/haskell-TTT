import Board
import Test.HUnit

testNewEmptyBoard = TestCase (assertEqual "should return new 9 space empty board"(take 9 (repeat empty)) (newBoard 3))

tests = TestList [TestLabel "New Board" testNewEmptyBoard ]


