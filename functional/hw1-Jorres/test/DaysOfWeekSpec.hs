module DaysOfWeekSpec where

import           DaysOfWeek

import           Test.Hspec

mainDaysOfWeekSpec :: IO ()
mainDaysOfWeekSpec = hspec $ do
  describe "DaysOfWeek tests" $ do
    describe "nextDay tests" $ do
      it "Tuesday after Monday" $ do
        nextDay Monday `shouldBe` Tuesday
      it "Monday after Sunday" $ do
        nextDay Sunday `shouldBe` Monday

    describe "afterDays tests" $ do
      it "Friday is 0 days after Friday" $ do
        afterDays Friday 0 `shouldBe` Friday
      it "Tuesday is 8 days after Monday" $ do
        afterDays Monday 8 `shouldBe` Tuesday

    describe "isWeekend tests" $ do
      it "Sunday is weekend" $ do
        isWeekend Sunday `shouldBe` True
      it "Saturday is weekend" $ do
        isWeekend Saturday `shouldBe` True
      it "Monday is not weekend :(" $ do
        isWeekend Monday `shouldBe` False

    describe "daysToParty tests" $ do
      it "Friday is the day of party, days = 0" $ do
        daysToParty Friday `shouldBe` 0
      it "Monday is 4 days from party aka Friday" $ do
        daysToParty Monday `shouldBe` 4
        
