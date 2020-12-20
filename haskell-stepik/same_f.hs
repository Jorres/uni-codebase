module Same where

f :: a -> a -> b -> a -> a
f a b c d = a

iden :: a -> a
iden a = a

f1 :: a -> a -> b -> a -> a
f1 a b c = iden

biden :: b -> a -> a
biden b = iden

f2 :: a -> a -> b -> a -> a
f2 a b = biden

triden :: a -> b -> a -> a
triden c = biden

f3 :: a -> a -> b -> a -> a
f3 a = triden
