module StringSumSpec where

import           StringSum

import           Test.Hspec

mainStringSumSpec :: IO ()
mainStringSumSpec = hspec $ do
    describe "stringSum tests:" $ do
      it "\"123\" == Just 123" $ do
        stringSum "123" `shouldBe` Just 123
      it "\"1 2 3 4 5 6\" == Just 21" $ do
        stringSum "1 2 3 4 5 6" `shouldBe` Just 21
      it "\"123 abc\" == Nothing" $ do
        stringSum "123 abc" `shouldBe` Nothing
      it "\"123abc\" == Nothing" $ do
        stringSum "123abc" `shouldBe` Nothing

