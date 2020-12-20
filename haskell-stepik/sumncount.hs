module Sumncount where 

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x = helper (abs x) 0 0
    
helper :: Integer -> Integer -> Integer -> (Integer, Integer)
helper x digits sum | x < 10 = (digits + 1, sum + x)
                    | otherwise = helper (x `div` 10) (digits + 1) (sum + x `mod` 10)
