module NonEmptySpec where

import           NonEmpty

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

subj :: a -> NonEmpty a
subj st = st :| [st]

mainNonEmptySpec :: IO ()
mainNonEmptySpec = hspec $ do
    describe "Functor:" $ do
      it "fmap with (+1)" $ 
         fmap (+1) (subj 1)`shouldBe` (subj 2)
      it "fmap with ([] -> [[]])" $ 
         fmap (\x -> [x]) (subj 1)`shouldBe` (subj [1])
    describe "Applicative:" $ do
      it "apply (+1) and (+2)" $
         ((+1) :| [(+2)]) <*> (1 :| [2]) `shouldBe`
            (2 :| [3, 3, 4])
    describe "Foldable:" $ do
      prop "fold with (+)" $ \a b len -> (len >= 0) ==>
        (foldr ((+) :: Int -> Int -> Int) 0 (a :| take len (repeat b))) 
            `shouldBe` (a + b * len)
    describe "Monad:" $ do
      it "bind with repeat" $ do
        ((1 :| [2]) >>= (\a -> a :| (take 2 $ repeat a))) `shouldBe`
            (1 :| [1, 1, 2, 2, 2])
