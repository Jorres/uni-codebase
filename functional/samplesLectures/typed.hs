{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
    
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE KindSignatures #-}

infixr 2 :^ 

data HList :: [*] -> * where
    Empty :: HList '[]
    (:^) :: a -> HList t -> HList (a ': t)
