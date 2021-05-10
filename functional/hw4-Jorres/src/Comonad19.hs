module Comonad19 where

import Control.Comonad
import Control.DeepSeq
import Control.Monad
import System.Random

data ListZipper a = LZ [a] a [a]

left :: ListZipper a -> ListZipper a
left (LZ (a : as) x bs) = LZ as a (x : bs)
left _ = error "no elements remaining to the left"

right :: ListZipper a -> ListZipper a
right (LZ as x (b : bs)) = LZ (x : as) b bs
right _ = error "no elements remaining to the right"

write :: a -> ListZipper a -> ListZipper a
write x (LZ ls _ rs) = LZ ls x rs

toList :: ListZipper a -> Int -> [a]
toList (LZ ls x rs) n = reverse (take n ls) ++ (x : take n rs)

prettyPrintLifeList2 :: Show a => [[a]] -> String
prettyPrintLifeList2 = foldr (\x -> (++) (show x ++ "\n")) ""

instance Functor ListZipper where
  fmap f (LZ ls x rs) = LZ (map f ls) (f x) (map f rs)

iterateTail :: (a -> a) -> a -> [a]
iterateTail f = tail . iterate f

genericMove ::
  (z a -> z a) ->
  (z a -> z a) ->
  z a ->
  ListZipper (z a)
genericMove f g e = LZ (iterateTail f e) e (iterateTail g e)

instance Comonad ListZipper where
  extract (LZ _ x _) = x
  duplicate = genericMove left right

newtype Grid a = Grid {unGrid :: ListZipper (ListZipper a)}

mvU :: Grid a -> Grid a
mvU (Grid g) = Grid (left g)

mvD :: Grid a -> Grid a
mvD (Grid g) = Grid (right g)

mvL :: Grid a -> Grid a
mvL (Grid g) = Grid (fmap left g)

mvR :: Grid a -> Grid a
mvR (Grid g) = Grid (fmap right g)

horizontal :: Grid a -> ListZipper (Grid a)
horizontal = genericMove mvL mvR

vertical :: Grid a -> ListZipper (Grid a)
vertical = genericMove mvU mvD

instance Functor Grid where
  fmap f (Grid g) = Grid $ fmap (fmap f) g

gridWrite :: a -> Grid a -> Grid a
gridWrite x (Grid g) = Grid $ write newLine g
  where
    oldLine = extract g
    newLine = write x oldLine

gridWriteLine :: ListZipper a -> Grid a -> Grid a
gridWriteLine ln (Grid g) = Grid $ write ln g

instance Comonad Grid where
  extract (Grid g) = extract $ extract g
  duplicate = Grid . fmap horizontal . vertical

data DiseaseStatus = ACTIVE_ILL | PASSIVE_ILL | IMMUNE | HEALTHY
  deriving (Eq)

instance Show DiseaseStatus where
  show ACTIVE_ILL = "A"
  show PASSIVE_ILL = "P"
  show IMMUNE = "I"
  show HEALTHY = "H"

data Cell = Cell DiseaseStatus StdGen Int

instance Show Cell where
  show (Cell st _ _) = show st

neighbours :: [Grid a -> Grid a]
neighbours = horizontals ++ verticals
  where
    horizontals = [mvL, mvR]
    verticals = [mvU, mvD]

illCount :: [Cell] -> Int
illCount = length . filter (\(Cell st _ _) -> st == ACTIVE_ILL || st == PASSIVE_ILL)

illNeighbours :: Grid Cell -> Int
illNeighbours g =
  illCount $
    map (\direction -> extract $ direction g) neighbours

mutate :: Int -> Int -> Cell -> Cell
mutate imm _ (Cell ACTIVE_ILL g 0) = Cell IMMUNE g imm
mutate _ vis (Cell PASSIVE_ILL g 0) = Cell ACTIVE_ILL g vis
mutate _ _ (Cell IMMUNE g 0) = Cell HEALTHY g 0
mutate _ _ p@(Cell HEALTHY g 0) = p
mutate _ _ p@(Cell st g rem) = Cell st g (rem - 1)

wearOff :: Int -> Int -> Grid Cell -> Cell
wearOff imm inc g = mutate imm inc (extract g)

tryComonad19 :: Int -> Double -> Int -> Cell -> Cell
tryComonad19 _ _ 0 p = p
tryComonad19 inc threshold neighbours p@(Cell _ g rem) =
  let (prop, newg) = random g
   in if prop < threshold
        then Cell PASSIVE_ILL newg inc
        else tryComonad19 inc threshold (neighbours - 1) (Cell PASSIVE_ILL newg inc)

newStatuses :: Int -> Double -> Grid Cell -> Cell
newStatuses vis threshold g =
  let p@(Cell st gen l) = extract g
   in case st of
        HEALTHY -> tryComonad19 vis threshold (illNeighbours g) p
        _ -> p

evolve :: Double -> Int -> Int -> Int -> Grid Cell -> Grid Cell
evolve p inc vis imm g = extend (newStatuses inc p) $ extend (wearOff imm vis) g

printGrid :: Show a => Int -> Grid a -> IO ()
printGrid sz (Grid g) = do
  let t = (`toList` sz) <$> toList g sz
  putStrLn $ prettyPrintLifeList2 t
  return ()

printGrid2 :: Show a => Grid a -> IO ()
printGrid2 = printGrid 2
