{-# LANGUAGE InstanceSigs #-}

module Tree where

data Tree t
  = Leaf
  | Node [t] (Tree t) (Tree t)
  deriving (Show)

isEmpty :: Ord t => Tree t -> Bool
isEmpty Leaf = True
isEmpty _ = False

countSize :: Ord t => Tree t -> Int
countSize Leaf = 0
countSize (Node _ l r) = 1 + countSize l + countSize r

findElement :: Ord t => Tree t -> t -> Bool
findElement Leaf _ = False
findElement cur@(Node (x : _) l r) req
  | x == req = True
  | x < req = findElement r req
  | otherwise = findElement l req

insertElement :: Ord t => Tree t -> t -> Tree t
insertElement Leaf a = Node [a] Leaf Leaf
insertElement (Node lst@(x : _) l r) a
  | x == a = Node (a : lst) l r
  | x < a = Node lst l (insertElement r a)
  | otherwise = Node lst (insertElement l a) r

deleteElement :: Ord t => Tree t -> t -> Tree t
deleteElement Leaf _ = Leaf
deleteElement (Node lst@(x : xs) l r) a
  | x == a && xs == [] =
    let merge :: Ord t => Tree t -> Tree t -> Tree t
        merge Leaf r = r -- merge Leaf = id
        merge l Leaf = l
        merge (Node lst ll lr) r = Node lst (merge ll lr) r
     in merge l r
  | x == a = Node xs l r
  | x < a = Node lst l (deleteElement r a)
  | otherwise = Node lst (deleteElement l a) r

fromList :: Ord t => [t] -> Tree t
fromList [] = Leaf
fromList (x : xs) = insertElement (fromList xs) x

instance Foldable Tree where
  foldr :: (t -> b -> b) -> b -> Tree t -> b
  foldr _ st Leaf = st
  foldr f st (Node lst l r) =
    foldr f (foldr f (foldr f st r) lst) l
  foldMap :: Monoid m => (t -> m) -> Tree t -> m
  foldMap _ Leaf = mempty
  foldMap f (Node lst l r) = foldMap f l <> foldMap f lst <> foldMap f r
