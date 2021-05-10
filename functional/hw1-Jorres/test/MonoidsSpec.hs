module MonoidsSpec where

import           Monoids

import           Data.Monoid
import           Test.Hspec

mainMonoidsSpec :: IO ()
mainMonoidsSpec = hspec $ do
  describe "maybeConcat tests" $ do
    it "maybeConcat a lot of Nothings is []" $ do
      maybeConcat [Nothing :: Maybe [Integer], Nothing, Nothing]
        `shouldBe` []
    it "maybeConcat a regular Maybe" $ do
      maybeConcat [(Just [1 :: Integer]), (Just [2]), (Just [3])]
        `shouldBe` [1, 2, 3]

  describe "eitherConcat tests" $ do
    it "eitherConcat a lot of Lefts is (left, mempty)" $ do
      eitherConcat [Left (Sum 3), Left (Sum 5)]
        `shouldBe` ((Sum 8), mempty :: Sum Integer)
    it "eitherConcat a lot of Rights is (mempty, right)" $ do
      eitherConcat [Right (Product 3), Right (Product 5)]
        `shouldBe` (mempty :: Sum Integer, (Product 15))
    it "proper eitherConcat with both arguments" $ do
      eitherConcat [Left (Sum 3), Right [1,2,3], Left (Sum 5), Right [4,5]]
        `shouldBe` ((Sum 8), [1, 2, 3, 4, 5])
