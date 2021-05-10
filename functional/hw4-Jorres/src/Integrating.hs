module Integrating where

import Control.DeepSeq
import Control.Parallel.Strategies
import System.Random

integral :: Double -> Double
integral x = 1.0 / tan (x ^ 2) - cos x

condense :: Double -> Double -> [Double] -> [Double]
condense _ _ [] = []
condense l r (x : xs) = l + (r - l) * x : condense l r xs

processor :: Double -> Double -> Int -> Int -> Double
processor l r n id = sum (map integral (condense l r $ take n $ randoms $ mkStdGen id)) / fromIntegral n

seqIntegrate :: Double -> Double -> Int -> Double
seqIntegrate l r n = processor l r n 1

myParMap :: (a -> b) -> [a] -> Eval [b]
myParMap f [] = return []
myParMap f (a : as) = do
  b <- rpar (f a)
  bs <- myParMap f as
  return (b : bs)

parIntegrate :: Double -> Double -> Int -> Int -> Double
parIntegrate l r n t
  | n `mod` t /= 0 = error "The amount of work is not equally distributable between threads"
  | otherwise = sum $ runEval $ myParMap (processor l r (n `div` t)) (take t [0 ..])
