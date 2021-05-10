module MovingAverage where

import Control.Monad.State

data MyState = MyState {
    real  :: [Int],
    trail :: [Int],
    i     :: Int
}

moving :: Int -> [Int] -> [Double]
moving n lst = evalState (movingh n []) (MyState lst lst 0)

movingh :: Int -> [Double] -> State MyState [Double]
movingh n res = do
               realM <- gets real
               if realM == [] then 
                 return (reverse res)
               else do
                 iN   <- gets i
                 iF   <- gets (fromIntegral . i)
                 next <- gets (fromIntegral . head . real)
                 last <- gets (fromIntegral . head . trail)
                 let prev = if res == [] then 0 else head res
                 let nF = fromIntegral n
                 let cur = (if iN < n then 
                                prev * (iF / (iF + 1)) + (1 / (iF + 1)) * next
                             else 
                                prev + (1 / nF) * ((next - last))
                           ) : res 
                 modify (\old@MyState 
                              {real = realO, trail = trailO, i = iO} -> 
                              old {real = tail realO, trail = if iO < n then trailO else tail trailO, i = iO + 1}) 
                 (movingh n cur)
