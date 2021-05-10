module GroupsSpec where

import           Groups

import           Data.Semigroup hiding (Endo)
import           Test.Hspec

listof :: [Integer] -> NonEmpty Integer
listof (x:xs) = x :| xs

mainGroupsSpec :: IO ()
mainGroupsSpec = hspec $ do
  describe "Semigroup for NonEmpty" $ do
    describe "Checking <>" $ do
      it "Check two lists of one element give one list of two" $ do
        (listof [1]) <> (listof [2]) `shouldBe` (listof [1, 2])
      it "Check two longer lists" $ do
        (listof [1, 2, 3]) <> (listof [4, 5, 6])
          `shouldBe` (listof [1, 2, 3, 4, 5, 6])
      it "Check associativity of <>" $ do
        ((listof [1]) <> (listof [2])) <> (listof [3])
        `shouldBe`
        (listof [1]) <> ((listof [2]) <> (listof [3]))

  describe "Semigroup for ThisOrThat" $ do
    describe "Checking <>" $ do
      it "this <> this = this with left" $ do
        ((This 1) :: ThisOrThat Int Int) <> (This 2) `shouldBe` (This 1)
      it "that <> that = that with left" $ do
        ((That 1) :: ThisOrThat Int Int) <> (That 2) `shouldBe` (That 1)

      it "that <> this = both" $ do
        ((That 1) :: ThisOrThat Int Int) <> (This 2) `shouldBe` (Both 2 1)
      it "this <> that = both" $ do
        ((This 1) :: ThisOrThat Int Int) <> (That 2) `shouldBe` (Both 1 2)

      it "this <> both = both with a from this" $ do
        (This 1) <> (Both 2 3) `shouldBe` (Both 1 3)
      it "that <> both = both with b from that" $ do
        (That 1) <> (Both 2 3) `shouldBe` (Both 2 1)

    describe "Semigroup for Name" $ do
      describe "Checking <>" $ do
        it "root <> name = root.name" $ do
            Name "root" <> Name "name" `shouldBe` Name "root.name"
        it "root <> empty-string = root" $ do
            Name "root" <> Name "" `shouldBe` Name "root"

    describe "Monoid for Name" $ do
      describe "Checking mempty" $ do
        it "mempty shouldBe Name \"\"" $ do
            (mempty :: Name) `shouldBe` Name ""

    -- describe "Semigroup for Endo" $ do
    --   describe "Checking <>" $ do
    --     it "(x -> x + 1) <> (x -> x * 2)= (x -> (x + 1) * 2)" $ do
    --         (((Endo {getEndo = (\x -> x + 1)}) :: Endo Integer) <>
    --           (Endo {getEndo = (\x -> x * 2)})) 10 `shouldBe` 22
    --     -- it "(\x -> x ++ x) <> (\x -> head x) = (\x -> head (x ++ x))" $ do

    describe "Monoid for Endo" $ do
      describe "Checking mempty" $ do
        it "mempty shouldBe id " $ do
            (getEndo (mempty :: Endo Integer)) 1 `shouldBe` 1

