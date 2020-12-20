groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems l = helper l [] [] 0

helper [] acc res n = acc : res
helper (x : xs) acc res n = if n == 0 || head acc == x 
                            then helper xs (x : acc) res (n + 1) 
                            else helper xs (x : []) (acc : res) (n + 1)
