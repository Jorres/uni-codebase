module BreakSpec where

import           Break

import           Test.Hspec

nonEmptyList :: [a] -> NonEmptyList a
nonEmptyList (x:xs) = NonEmptyList x xs

mainBreakSpec :: IO ()
mainBreakSpec = hspec $ do
  describe "splitOn tests" $ do
    it "Split string with '/'" $ do
      splitOn '/' "path/to/find" `shouldBe`
        (nonEmptyList ["path", "to", "find"])
    it "Split list with 0" $ do
      splitOn 0 [1, 0, 2, 0, 3, 0, 4] `shouldBe`
        (nonEmptyList [[1], [2], [3], [4]])

  describe "joinWith tests" $ do
    it "Join string with '/'" $ do
      joinWith '/' (nonEmptyList ["path", "to", "find"]) `shouldBe`
        "path/to/find"
    it "Join list with [0]" $ do
      joinWith 0 (nonEmptyList [[1], [2], [3], [4]]) `shouldBe`
        [1, 0, 2, 0, 3, 0, 4]

