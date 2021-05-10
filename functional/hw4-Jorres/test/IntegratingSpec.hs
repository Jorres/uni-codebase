module IntegratingSpec where

import Criterion.Main
import Integrating
import Test.Hspec

mainIntegratingSpec :: IO ()
mainIntegratingSpec =
  defaultMain
    [ bgroup
        "sequential integral"
        [ bench "1e4" $ whnf (seqIntegrate 0.1 1.7) (10 ^ 4),
          bench "1e6" $ whnf (seqIntegrate 0.1 1.7) (10 ^ 6)
        ],
      bgroup
        "parallel integral 10 sparks"
        [ bench "1e4" $ whnf (parIntegrate 0.1 1.7 (10 ^ 4)) 10,
          bench "1e6" $ whnf (parIntegrate 0.1 1.7 (10 ^ 6)) 10
        ],
      bgroup
        "parallel integral 100 sparks (expected win should be less than with 10 sparks)"
        [ bench "1e4" $ whnf (parIntegrate 0.1 1.7 (10 ^ 4)) 100,
          bench "1e6" $ whnf (parIntegrate 0.1 1.7 (10 ^ 6)) 100
        ]
    ]
