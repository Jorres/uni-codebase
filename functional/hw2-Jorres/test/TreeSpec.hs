module TreeSpec where

import           Tree

import           Test.Hspec

simpleTree :: Tree Int
simpleTree = Leaf 1

bigTreeWithVal :: a -> Tree a
bigTreeWithVal val = Branch (Branch (Leaf val) 
                                    (Leaf val)) 
                     (Leaf val)

mainTreeSpec :: IO ()
mainTreeSpec = hspec $ do
    describe "Tree functor tests:" $ do
      it "Check identity: fmap id Leaf 1 == Leaf 1" $ do
        fmap id simpleTree `shouldBe` simpleTree
      it "Check simple f = (*2) multiplies node values" $ do
        fmap (*2) (bigTreeWithVal 1) `shouldBe` 
                  (bigTreeWithVal 2)
    describe "Tree foldable tests:" $ do
      it "Fold with sum ((1, 1) 1) == 3" $ do
        foldr (\a b -> a + b) 0 (bigTreeWithVal 1) `shouldBe` 3
      it "Fold with toString and concat ((1, 1), 1) == 111" $ do
        foldr (\a b -> (show a) ++ b) "" (bigTreeWithVal "1") `shouldBe` 
                                     "\"1\"\"1\"\"1\""

