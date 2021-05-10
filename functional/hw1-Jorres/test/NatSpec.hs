module NatSpec where

import           Nat

import           Test.Hspec

zero :: Nat
zero = Z

one :: Nat
one = S Z

two :: Nat
two = S (S Z)

three :: Nat
three = S (S (S Z))

four :: Nat
four = S (S (S (S Z)))

five :: Nat
five = S (S (S (S (S Z))))

mainNatSpec:: IO ()
mainNatSpec = hspec $ do
  describe "Nat (peano) tests" $ do
    describe "Tests for sum" $ do
      it "0 + 1 = 1" $ do
        zero + one `shouldBe` one
      it "1 + 1 = 2" $ do
        one + one `shouldBe` two

    describe "Tests for subtraction" $ do
      it "1 - 0 = 1" $ do
        one - zero `shouldBe` one
      it "1 - 1 = 0" $ do
        one - one `shouldBe` zero

    describe "Tests for product" $ do
      it "1 * 0 = 0" $ do
        one * zero `shouldBe` zero
      it "1 * 1 = 1" $ do
        one * one `shouldBe` one
      it "1 * 2 = 2" $ do
        one * two `shouldBe` two
      it "2 * 2 = 4" $ do
        two * two `shouldBe` four

    describe "Tests for fromInteger" $ do
      it "fromInteger 2 = S(S(Z))" $ do
        ((fromInteger 2) :: Nat) `shouldBe` two
      it "fromInteger 0 shouldBe Z" $ do
        ((fromInteger 0) :: Nat) `shouldBe` Z

    describe "Tests for toInteger" $ do
      it "toInteger Z shouldBe 0" $ do
        (Nat.toInteger zero) `shouldBe` 0
      it "toInteger S(S(Z)) shouldBe 2" $ do
        (Nat.toInteger two) `shouldBe` 2

    describe "Tests for equality" $ do
      it "1 == 0 = False" $ do
        one == zero `shouldBe` False
      it "1 == 1 = True" $ do
        one == one `shouldBe` True
      it "one /= one = False" $ do
        one /= one `shouldBe` False

    describe "Tests for order" $ do
      it "1 <= 0 = False" $ do
        one <= zero `shouldBe` False
      it "0 <= 1 = True" $ do
        zero <= one `shouldBe` True

    describe "Tests for compare" $ do
      it "compare 1 0 shouldBe GT" $ do
        compare one zero `shouldBe` GT
      it "compare zero one shouldBe LT" $ do
        compare zero one `shouldBe` LT

    describe "Tests for oddity" $ do
      it "isOdd 0 shouldBe False" $ do
        isOdd zero `shouldBe` False
      it "isOdd 3 shouldBe True" $ do
        isOdd three `shouldBe` True

    describe "Tests for natural division" $ do
      it "natDiv 1 4 = 0" $ do
        natDiv one four `shouldBe` zero
      it "natDiv 4 2 = 2" $ do
        natDiv four two `shouldBe` two

    describe "Tests for modulus" $ do
      it "natMod 1 4 = 1" $ do
        natMod one four `shouldBe` one
      it "natMod 4 2 = 0" $ do
        natMod four two `shouldBe` zero
