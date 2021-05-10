module ExpressionSpec where

import           Expression

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

e :: Int -> Expr
e i = Expr i


-- prop_nonZeroDivision :: Int -> Int -> Property
prop_nonZeroDivision a b = (b /= 0) ==> (eval(Div (e a) (e b)) `shouldBe` Right (div a b))

prop_nonNegPower a b = (b >= 0) ==> (eval(Pow (e a) (e b)) `shouldBe` Right (a ^ b))
prop_negPower a b = (b < 0) ==> (eval(Pow (e a) (e b)) `shouldBe` Left NegativePow)

mainExpressionSpec :: IO ()
mainExpressionSpec = hspec $ do
    describe "Expression + tests:" $ do
      prop "a + b" $ \a b ->
        eval(Sum (Expr a) (Expr b)) `shouldBe` Right (a + b)
      prop "a + b + c" $ \a b c ->
        eval(Sum(Sum (e a) (e b)) (e c)) `shouldBe` Right (a + b + c)
    describe "Expression - tests:" $ do
      prop "a - b" $ \a b ->
        eval(Sub (e a) (e b)) `shouldBe` Right (a - b)
      prop "a - b - c" $ \a b c ->
        eval(Sub (Sub (e a) (e b)) (e c)) `shouldBe` Right (a - b - c)
    describe "Expression * tests:" $ do
      prop "a * b" $ \a b ->
        eval(Mul (e a) (e b)) `shouldBe` Right (a * b)
      prop "a * b * c" $ \a b c ->
        eval(Mul(Mul (e a) (e b)) (e c)) `shouldBe` Right (a * b * c)
      prop "a * a == a ^ 2"  $ \a ->
        eval(Mul (e a) (e a)) `shouldBe` eval(Pow (e a) (e 2))
    describe "Expression / tests:" $ do
      prop "a / b, b /= 0" $ prop_nonZeroDivision
      prop "a / 0 == DivisionByZero" $ \a ->
        eval(Div (e a) (e 0)) `shouldBe` Left DivisionByZero
    describe "Expression ^ tests:" $ do
      prop "a ^ b, b >= 0" $ prop_nonNegPower
      prop "a ^ b, b < 0" $ prop_negPower
