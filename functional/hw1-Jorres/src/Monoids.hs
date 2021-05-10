module Monoids where

maybeConcat :: [Maybe [a]] -> [a]
maybeConcat = foldr helper []
                where
                    helper (Just val) acc = val ++ acc
                    helper (Nothing) acc  = acc

eitherConcat :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
eitherConcat = foldr f (mempty, mempty)
                where
                    f (Left a) (l, r)  = ((a <> l), r)
                    f (Right a) (l, r) = (l, (a <> r))

