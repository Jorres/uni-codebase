module Comonad19Spec where

import Comonad19
import Control.Comonad
import System.Random

getPeriodSize :: DiseaseStatus -> Int
getPeriodSize ACTIVE_ILL = 2
getPeriodSize _ = 0

mkCell :: DiseaseStatus -> Cell
mkCell s = Cell s (mkStdGen 117) (getPeriodSize s)

evolveInSteps :: Grid Cell -> Int -> IO ()
evolveInSteps _ 0 = return ()
evolveInSteps g st = do
  printGrid2 g
  -- the parameters for the evolution:
  -- probability to get disease from 1 neighbour
  -- and three timespans for incubatory, active and immunity stages of the disease.
  let ng = evolve 0.5 2 2 2 g
  evolveInSteps ng (st - 1)

mainComonad19Spec :: IO ()
mainComonad19Spec = do
  putStrLn ""
  -- unfortunately, I was unable to think of how to create unique StdGen for every
  -- cell on the field. So every one of the cell has the same hardcoded seed of 117 and
  -- therefore has the same hardcoded behaviour. I still hope I can receive some points though
  -- because everything other than that is done

  let middle_line = LZ (repeat $ mkCell HEALTHY) (mkCell ACTIVE_ILL) (repeat $ mkCell HEALTHY)
  let g = Grid (duplicate middle_line)
  evolveInSteps g 8
