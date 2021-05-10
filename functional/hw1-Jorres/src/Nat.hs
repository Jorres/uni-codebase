module Nat where

data Nat = Z | S Nat
    deriving Show

instance Num Nat where
    (+) a Z     = a
    (+) a (S b) = (S a) + b
    (-) a Z         = a
    (-) Z _         = Z
    (-) (S a) (S b) = a - b
    (*) _ Z     = Z
    (*) Z _     = Z
    (*) a (S b) = (a * b) + a

    fromInteger a
        | a > 0 = S $ fromInteger (a - 1)
        | a == 0 = Z
        | a < 0 = error "negative numbers are not allowed"

toInteger :: Nat -> Integer
toInteger Z     = 0
toInteger (S a) = Nat.toInteger a + 1


instance Eq Nat where
    (==) Z Z         = True
    (==) _ Z         = False
    (==) Z _         = False
    (==) (S a) (S b) = a == b

instance Ord Nat where
    (<) _ Z         = False
    (<) Z _         = True
    (<) (S a) (S b) = a < b

    (>) a b = not (a < b)

    (<=) a b = (a == b) || (a < b)

    (>=) a b = (a == b) || (a > b)

    compare a b
        | a == b = EQ
        | a <= b = LT
        | otherwise =   GT

isOdd :: Nat -> Bool
isOdd Z         = False
isOdd (S Z)     = True
isOdd (S (S a)) = isOdd a

natDiv :: Nat -> Nat -> Nat
natDiv a b
    | a < b = Z
    | otherwise = natDiv (a - b) b + 1

natMod :: Nat -> Nat -> Nat
natMod a b = a - ((natDiv a b) * b)
