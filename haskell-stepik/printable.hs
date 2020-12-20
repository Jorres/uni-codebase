module Printable where

class Printable tp where
    toString :: tp -> [Char]

instance Printable Bool where
    toString True = "True"
    toString False = "False"

instance Printable () where
    toString () = "unit type"

instance (Printable a, Printable b) => Printable (a, b) where
    toString p = "(" ++ toString (fst p) ++ "," ++ toString (snd p) ++ ")"
