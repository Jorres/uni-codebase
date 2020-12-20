perms :: [a] -> [[a]]
perms [] = [[]]
perms (x : xs) = concatMap (insertFirst x [] []) (perms xs)

insertFirst x res start [] = (start ++ [x]) : res
insertFirst x res start (y : ys) = insertFirst x ((start ++ (x : y : ys)) : res) (start ++ [y]) ys
