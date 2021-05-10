module NonEmpty where

data NonEmpty a = a :| [a]
    deriving (Show, Eq)

instance Functor NonEmpty where
    fmap f (x :| xs) = f x :| fmap f xs

instance Applicative NonEmpty where
    pure x = x :| []
    (<*>) (f :| fs) (x :| xs) = f x :| 
                                (fmap f xs ++ 
                                (fs <*> [x]) ++ 
                                (fs <*> xs))

instance Foldable NonEmpty where
    foldr f acc (x :| xs) = f x (foldr f acc xs)

instance Traversable NonEmpty where
   sequenceA (x :| xs) = (:|) <$> x <*> sequenceA xs

instance Monad NonEmpty where
    return x = x :| []
    (>>=) (x :| xs) f = fr :| (frs ++ concatMap helper xs)
                  where 
                    (fr :| frs) = f x
                    helper x = fr : frs
                        where
                            (fr :| frs) = f x
