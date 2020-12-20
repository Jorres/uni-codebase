module Test where

dist :: (Double, Double) -> (Double, Double) -> Double
dist p1 p2 = sqrt (((fst p1) - (fst p2)) ^ 2 + ((snd p1) - (snd p2)) ^ 2)

seqA :: Integer -> Integer

seqA n | n == 0 = 1
       | n == 1 = 2
       | n == 2 = 3
       | otherwise = let 
            helper :: Integer -> Integer -> Integer -> Integer 
                                  -> Integer
            helper a b c 0 = c
            helper a b c n = helper b c (c + b - 2 * a) (n - 1)
         in helper 1 2 3 (n - 2)
