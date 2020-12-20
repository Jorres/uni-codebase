data Odd = Odd Integer 
  deriving (Eq, Show)

instance Enum Odd where
    -- succ     :: Odd -> Odd
    succ (Odd a) = Odd(toInteger a + 2)

    -- pred     :: Odd -> Odd
    pred (Odd a) = Odd(toInteger a - 2)

    -- toEnum         :: Int -> Odd
    toEnum a = Odd (fromIntegral (2 * a - 1))

    -- fromEnum       :: Odd -> Int
    fromEnum (Odd a) = fromIntegral (toInteger (div (a + 1) 2))

    -- enumFrom       :: Odd -> [Odd]
    enumFrom s = enumFromThen s (succ s)

    -- enumFromThen   :: Odd -> Odd -> [Odd]
    enumFromThen (Odd s) (Odd f) = (Odd si) : (enumFromThen (Odd fi) (Odd (fi + fi - si)))
        where
            si = toInteger s
            fi = toInteger f

    -- enumFromTo     :: Odd -> Odd -> [Odd]
    enumFromTo s f = enumFromThenTo s (succ s) f

    -- enumFromThenTo :: Odd -> Odd -> Odd -> [Odd]  -- [n,n'..m]
    enumFromThenTo (Odd s) (Odd f) (Odd e) | (si > fi && si < ei) || (si < fi && si > ei) = []
                                           | otherwise = (Odd si) : (enumFromThenTo (Odd fi) (Odd (fi + fi - si)) (Odd ei))
       where
            si = toInteger s
            fi = toInteger f
            ei = toInteger e
