factor :: Int -> [Int]
factor n = l ++ map (\x -> div n x) l
            where 
                l = [x | x <- [1..getBorder n], mod n x == 0]  
                getBorder = ceiling . sqrt . fromIntegral
