module TreeSpec where

import           Tree

import           Data.Foldable
import           Data.Monoid
import           Test.Hspec

mainTreeSpec:: IO ()
mainTreeSpec = hspec $ do
  describe "BST tests" $ do
    describe "Tests for emptiness" $ do
      it "Leaf is empty" $ do
        (isEmpty (Leaf :: Tree Integer)) `shouldBe` True
      it "Some non-trivial tree is not empty" $ do
        (isEmpty createSimpleTree) `shouldBe` False

    describe "Tests for emptiness" $ do
      it "Empty tree has size = 0" $ do
        (countSize (Leaf :: Tree Integer)) `shouldBe` 0
      it "Some non-trivial tree has size = 3" $ do
        (countSize createSimpleTree) `shouldBe` 3

    describe "Tests for finding the element" $ do
      it "Finding element in empty tree returns False" $ do
        (findElement Leaf (1 :: Integer)) `shouldBe` False
      it "Finding non-existent element in tree returns False" $ do
        (findElement createSimpleTree (4 :: Integer)) `shouldBe` False
      it "Finding root element in tree returns True" $ do
        (findElement createSimpleTree (2 :: Integer)) `shouldBe` True
      it "Finding non-root element in tree returns True" $ do
        (findElement createSimpleTree (1 :: Integer)) `shouldBe` True

    describe "Tests for inserting element" $ do
      it "Inserting single element in empty tree works" $ do
        (findElement (insertElement Leaf (1 :: Integer)) 1) `shouldBe` True
      it "Inserting single element in a non-trivial tree works" $ do
        (findElement (insertElement createSimpleTree 4) 4) `shouldBe` True

    describe "Tests for deleting element" $ do
      it "Deleting root element from non-trivial tree works" $ do
        (findElement (deleteElement createSimpleTree 2) 2) `shouldBe` False
      it "Deleting non-root element from non-trivial tree works" $ do
        (findElement (deleteElement createSimpleTree 1) 1) `shouldBe` False

    describe "Tests for fromList" $ do
      it "Finding element in tree created from list returns True" $ do
        (findElement (fromList [1..10]) 2) `shouldBe` True
      it "Finding element not from the list in tree returns False" $ do
        (findElement (fromList [1..10]) 11) `shouldBe` False

    describe "Tests for instance Foldable" $ do
      it "Folding empty tree, getting []" $ do
        toList (Leaf :: Tree Integer) `shouldBe` []
      it "Folding non-trivial tree, getting list" $ do
        toList createSimpleTree `shouldBe` [1, 2, 3]
      it "Sorting [2, 5, 4, 2] via tree is [2, 2, 4, 5]" $ do
        toList (fromList [2, 5, 4, 2]) `shouldBe` [2, 2, 4, 5]

    describe "Tests for mapReduce aka foldMap" $ do
      it "foldMapping with (+) tree of Integers" $ do
        (foldMap (Sum . (+ 0)) createSimpleTree) `shouldBe` (Sum 6)


createSimpleTree :: Tree Integer
createSimpleTree = Node [2]
            (Node [1] Leaf Leaf)
            (Node [3] Leaf Leaf)

