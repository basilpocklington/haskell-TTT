import Test.HUnit

test1 = TestCase (assertEqual "This Thing" 1 1)

tests = TestList [TestLabel "test1" test1]


