{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Gadt where

import Data.Typeable

-- data AE = AENum Int | AE_SUM (AE Int) () 

data AEG a where
    AEG_BOOL :: Bool -> AEG Bool
    AEG_INT  :: Int -> AEG Int
    AEG_SUM  :: AEG Int -> AEG Int -> AEG Int
    AEG_GT   :: AEG Int -> AEG Int -> AEG Bool
    AEG_AND  :: AEG Bool -> AEG Bool -> AEG Bool

interpret :: AEG a -> a
interpret (AEG_INT n) = n
interpret _ = undefined

-- parse :: String -> Maybe (AEG a)
-- parse "1" = Just $ AEG_Int 1
-- parse "True" = Just $ AEG_BOOL True
-- parse _ = Nothing
--
data Wrapper where 
    Wrapper :: (Typeable a) => AEG a -> Wrapper

parse :: String -> Maybe Wrapper
parse "1" = Just $ Wrapper $ AEG_INT 1
parse "True" = Just $ Wrapper $ AEG_BOOL True
parse _ = Nothing


parseInt :: String -> Maybe (AEG Int)
parseInt s = parse s >>= (\(Wrapper (expr :: AEG t)) -> do
                            Refl <- eqT @t @Int
                            pure expr)

