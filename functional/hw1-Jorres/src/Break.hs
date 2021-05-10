module Break where

import           Data.List

data NonEmptyList a = NonEmptyList a [a]
    deriving (Show, Eq)

instance Foldable NonEmptyList where
    foldr f val (NonEmptyList a xs) = f a (foldr f val xs)

splitOn :: Eq a => a -> [a] -> NonEmptyList [a]
splitOn elem =
  foldr helper (NonEmptyList [] [])
    where
      helper cur (NonEmptyList a rest)
        | cur == elem = (NonEmptyList [] (a:rest))
        | otherwise = (NonEmptyList (cur:a) rest)

joinWith :: a -> NonEmptyList [a] -> [a]
joinWith elem =
  foldr helper []
    where
      helper cur []  = cur
      helper cur lst = cur ++ [elem] ++ lst

