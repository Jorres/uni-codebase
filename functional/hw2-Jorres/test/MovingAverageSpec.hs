module MovingAverageSpec where

import           MovingAverage
import           Test.Hspec


mainMovingAverageSpec :: IO ()
mainMovingAverageSpec = hspec $ do
    describe "Moving average :" $ do
      it "moving 4 [1, 5, 3, 8, 7, 9, 6]" $ do
        moving 4 [1, 5, 3, 8, 7, 9, 6] `shouldBe` 
            [1.0, 3.0, 3.0, 4.25, 5.75, 6.75, 7.5]
      it "moving 2 [1, 5, 3, 8, 7, 9, 6]" $ do
        moving 2 [1, 5, 3, 8, 7, 9, 6] `shouldBe` 
            [1.0, 3.0, 4.0, 5.5, 7.5, 8.0, 7.5]
