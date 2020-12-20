module Integrate where

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = divider f a b 1000
    where mod2 :: Integer -> Integer
          mod2 x = mod x 2
          div2 :: Integer -> Integer
          div2 x = div x 2
          divider :: (Double -> Double) -> Double -> Double -> Integer -> Double
          divider f a b segs | segs == 1 = (f ((a + b) / 2)) * (b - a)
                             | mod2 segs == 1 = divider f a ((a + b) / 2) (div2 segs + 1)
                                              + divider f ((a + b) / 2) b (div2 segs)
                             | otherwise = divider f a ((a + b) / 2) (div2 segs)
                                         + divider f ((a + b) / 2) b (div2 segs)
