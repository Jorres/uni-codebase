module StringSum where

import             Data.List
import             Text.Read

stringSum :: String -> Maybe Int
stringSum s = helper (traverse (\x -> readMaybe x) (words s))
                where 
                    helper Nothing   = Nothing
                    helper (Just xs) = Just (sum xs)


-- no traverse implementation, just for history:
--
-- stringSum :: String -> Maybe Int
-- stringSum str = foldl (\acc val -> 
--                         case acc of 
--                              Nothing -> Nothing
--                              Just x  -> case readMaybe val of 
--                                              Nothing -> Nothing
--                                              Just y -> Just (x + y)) 
--                        (Just 0) (words str)
