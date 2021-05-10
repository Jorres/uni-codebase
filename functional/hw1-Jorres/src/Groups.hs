{-# LANGUAGE InstanceSigs #-}

module Groups where

import           Data.Semigroup hiding (Endo)

data NonEmpty a = a :| [a]
    deriving (Show, Eq)

instance Semigroup (NonEmpty a) where
    (<>) :: NonEmpty a -> NonEmpty a -> NonEmpty a
    (<>) (x :| xs) (y :| ys) = (x :| (xs ++ (y:ys)))

data ThisOrThat a b = This a | That b | Both a b
    deriving (Show, Eq)

instance Semigroup (ThisOrThat x y) where
    (<>) :: ThisOrThat x y -> ThisOrThat x y -> ThisOrThat x y
    (<>) (This a) (This b)   = This a
    (<>) (That a) (That b)   = That a

    (<>) (That a) (This b)   = Both b a
    (<>) (This a) (That b)   = Both a b

    (<>) (This a) (Both b c) = Both a c
    (<>) (That a) (Both b c) = Both b a

    (<>) (Both a b) _        = Both a b

newtype Name = Name [Char]
    deriving (Show, Eq)

instance Semigroup Name where
    (<>) :: Name -> Name -> Name
    (<>) (Name s1) (Name s2)
            | s1 == "" = Name s2
            | s2 == "" = Name s1
            | otherwise = Name (s1 ++ ('.' : s2))

instance Monoid Name where
    mempty :: Name
    mempty = Name ""

newtype Endo a = Endo {
    getEndo :: a -> a
}

instance Semigroup (Endo a) where
    (<>) :: Endo a -> Endo a -> Endo a
    (<>) ea eb = Endo { getEndo = ((getEndo ea) . (getEndo eb)) }

instance Monoid (Endo a) where
    mempty :: Endo a
    mempty = Endo { getEndo = id }

