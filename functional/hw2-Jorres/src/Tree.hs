module Tree where

data Tree a
  = Branch (Tree a) (Tree a)
  | Leaf a
    deriving (Show, Eq)

instance Functor Tree where
    fmap f (Branch tl tr) = Branch (fmap f tl) (fmap f tr)
    fmap f (Leaf val)     = Leaf (f val)

instance Applicative Tree where
    pure = Leaf
    (<*>) (Leaf f) x     = fmap f x
    (<*>) (Branch l r) t = Branch (l <*> t) (r <*> t)

instance Foldable Tree where
    foldr f acc (Branch tl tr) = foldr f (foldr f acc tr) tl
    foldr f acc (Leaf val)     = f val acc

instance Traversable Tree where
    traverse f (Leaf x) = Leaf <$> f x
    traverse f (Branch l r) = Branch <$>
                traverse f l <*> traverse f r
