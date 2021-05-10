{-# LANGUAGE TypeFamilies #-}

module Main where

type family F a
type instance F Int = Char
type instance F a = a

f1 :: F Int
f1 = 'a'

f2 :: F Bool
f2 = True

main :: IO ()
main = putStrLn "compiled"
