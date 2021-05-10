infixr 1 ==>
(==>) ::  Bool -> Bool -> Bool
a ==> b = not a || b

applyTwice :: (Int -> Int) -> Int -> Int
applyTwice operation value = operation (operation value)

-- applyTwice (\x -> x - 1) 5 == 3
