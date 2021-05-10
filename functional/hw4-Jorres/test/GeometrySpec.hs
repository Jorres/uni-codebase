module GeometrySpec where

import Criterion.Main
import Geometry
import Test.Hspec

makeHalfRectangle :: Int -> Int -> Int -> Int -> [Point]
makeHalfRectangle _ _ _ 0 = []
makeHalfRectangle x step y rem = Point {x = x, y = y} : makeHalfRectangle (x + step) step y (rem - 1)

makeRectangleData n
  | (n `mod` 2) /= 0 = error "Test rectangle can be made only out of even number of points"
  | otherwise = makeHalfRectangle (t - 1) (-1) 1 t ++ makeHalfRectangle 0 1 0 t
  where
    t = n `div` 2

smallTest = makeRectangleData $ 10 ^ 4

bigTest = makeRectangleData $ 10 ^ 7

mp x y = Point {x = x, y = y}

mainGeometrySpec :: IO ()
mainGeometrySpec = do
  hspec $ do
    describe "Edges:" $
      it "Euclid distance: " $
        euclid (mp 1 1) (mp 0 0) `shouldBe` 2
    describe "Area:" $ do
      it "Naive area:" $
        doubleAreaNaive (makeRectangleData 8) `shouldBe` 6
      it "Strict area:" $
        doubleArea (makeRectangleData 8) `shouldBe` 6
    describe "Perimeter:" $ do
      it "Naive perimeter:" $
        perimeterNaive (makeRectangleData 8) `shouldBe` 8
      it "Strict perimeter:" $
        perimeter (makeRectangleData 8) `shouldBe` 8
  defaultMain
    [ bgroup
        "perimeter"
        [ bench "naive 1e4" $ whnf perimeterNaive smallTest,
          bench "naive 1e7" $ whnf perimeterNaive bigTest,
          bench "strict 1e4" $ whnf perimeter smallTest,
          bench "strict 1e7" $ whnf perimeter bigTest
        ],
      bgroup
        "area"
        [ bench "naive 1e4" $ whnf doubleAreaNaive smallTest,
          bench "naive 1e7" $ whnf doubleAreaNaive bigTest,
          bench "strict 1e4" $ whnf doubleArea smallTest,
          bench "strict 1e7" $ whnf doubleArea bigTest
        ]
    ]
