data MyClass = C1 | C2 a | C3 Int

instance Monad (MyClass Int) where
  (>>=) = undefined
  return = undefined
