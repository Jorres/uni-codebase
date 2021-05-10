{-# LANGUAGE BangPatterns #-}

module Geometry where

data Point = Point
  { x :: Int,
    y :: Int
  }
  deriving (Show, Eq)

action :: (Int -> Int -> Int) -> Point -> Point -> Point
action op p1 p2 = Point {x = op (x p1) (x p2), y = op (y p1) (y p2)}

plus :: Point -> Point -> Point
plus = action (+)

minus :: Point -> Point -> Point
minus = action (-)

scalarProduct :: Point -> Point -> Int
scalarProduct p1 p2 = x p + y p
  where
    p = action (*) p1 p2

euclid :: Point -> Point -> Int
euclid p1 p2 = (x p1 - x p2) ^ 2 + (y p1 - y p2) ^ 2

crossProduct :: Point -> Point -> Int
crossProduct p1 p2 = a - b
  where
    a = x p1 * y p2
    b = x p2 * y p1

iterateN :: Num a => (Point -> Point -> a) -> [Point] -> Point -> a
iterateN f [x] st = f x st
iterateN f (x : y : xs) st = f x y + iterateN f (y : xs) st

iterateS :: Num a => (Point -> Point -> a) -> [Point] -> Point -> a -> a
iterateS f [x] st !z = z + f x st
iterateS f (x : y : xs) st !z = iterateS f (y : xs) st (z + f x y)

perimeter :: [Point] -> Double
perimeter p = iterateS (\p1 p2 -> sqrt $ fromIntegral $ euclid p1 p2) p (head p) 0

perimeterNaive :: [Point] -> Double
perimeterNaive p = iterateN (\p1 p2 -> sqrt $ fromIntegral $ euclid p1 p2) p (head p)

doubleArea :: [Point] -> Int
doubleArea p = iterateS crossProduct p (head p) 0

doubleAreaNaive :: [Point] -> Int
doubleAreaNaive p = iterateN crossProduct p (head p)
