import Data.List

evenOnly :: [a] -> [a]
evenOnly = fst . foldr folder ([], False)
    where folder v (prev, b) | b         = (v : prev, False)
                             | otherwise = (prev, True)
