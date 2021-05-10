-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module A where

-- import Control.Lens

import Control.Monad.Identity
import Data.Functor.Const

-- data L a f = L {
--     view :: a -> f,
--     set :: a -> f -> a
-- }
-- over :: (L a f) -> (f -> f) -> a -> a
-- over l upd obj = set l obj (upd $ view l obj)

data MyNestedClass = MyNestedClass
  { _name :: String
  }
  deriving (Show)

-- makeLenses ''MyNestedClass

data MyClass = MyClass
  { _y :: MyNestedClass
  }

-- makeLenses ''MyClass
--
-- instance Functor (Const obj) where
--     fmap _ (Const objname) = Const objname
--
-- instance Functor (Identity a) where
--     fmap f (Identity val) = Identity $ f val

type MyLens o f = forall x. (Functor x) => (f -> x f) -> o -> x o

view :: o -> MyLens o f -> f
view obj l = getConst $ l (\old -> Const old) obj

set :: MyLens o f -> o -> f -> o
set l old to = runIdentity $ l (\obj -> Identity obj) old

myYLens :: MyLens MyClass MyNestedClass
myYLens upd obj = fmap (\new -> MyClass {_y = new}) (upd (_y obj))

myNameLens :: MyLens MyNestedClass String
myNameLens upd obj = fmap (\new -> MyNestedClass {_name = new}) (upd (_name obj))

myYNameLens :: MyLens MyClass String
myYNameLens = myYLens . myNameLens

--  (name -> f name) -> (MyClass -> f MyClass)

nested = MyNestedClass {_name = "Egor"}

myClass = MyClass {_y = nested}

main :: IO ()
main = putStrLn $ view myClass myYNameLens
