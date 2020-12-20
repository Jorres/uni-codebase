

replicatus :: Int -> a -> [a]

replicatus 0 _ = []
replicatus x a = a : (replicatus (x - 1) a)
