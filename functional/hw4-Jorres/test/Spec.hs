module Main where

import Comonad19Spec
import GeometrySpec
import IntegratingSpec

main :: IO ()
main = do
  mainGeometrySpec
  mainIntegratingSpec

-- mainComonad19Spec
