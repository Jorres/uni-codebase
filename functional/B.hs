{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module B where

import Control.Lens
import Data.Functor.Const
import Control.Monad.Identity

data Point = Point {
    _x :: Int,
    _y :: Int
}

data Unit = Unit {
    _health :: Int,
    _position :: Point
}

data Game = Game {
    _score :: Int,
    _units :: [Unit],
    _boss  :: Unit
}

score :: Lens' Game Int
score = lens _score (\game v -> game { _score = v })

units :: Lens' Game [Unit]
units = lens _units (\game v -> game { _units = v })

boss :: Lens' Game Unit
boss = lens _boss (\game v -> game { _boss = v })

health :: Lens' Unit Int
health = lens _health (\unit v -> unit { _health = v })

position :: Lens' Unit Point
position = lens _position (\unit v -> unit { _position = v })

x :: Lens' Point Int
x = lens _x (\point v -> point { _x = v })

y :: Lens' Point Int
y = lens _y (\point v -> point { _y = v })

initialState :: Game
initialState = Game {
    _score = 0,
    _units = [
        Unit {_health = 1, _position = Point {_x = 2, _y = 0}},
        Unit {_health = 1, _position = Point {_x = 0, _y = 0}},
        Unit {_health = 1, _position = Point {_x = 0, _y = 0}}
    ],
    _boss = Unit {_health = 1, _position = Point {_x = 0, _y = 0}}
}

main :: IO () 
main = putStrLn $ show $ initialState^..units.traversed.health
    
