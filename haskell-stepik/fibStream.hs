fibStream :: [Integer]
fibStream = [0, 1] ++ zipWith (+) fibStream (tail fibStream)

-- [ 0 0 1 1 2 3 5  ]
-- [ 0 1 1 2 3 5 8  ]
-- [ 0 1 2 3 5 8 13 ]

