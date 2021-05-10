data MyMaybe a = MyNothing
               | MyJust a 

maybeSecond :: [a] -> MyMaybe a
maybeSecond (_:a:_) = MyJust a
maybeSecond _ = MyNothing

showR :: MyMaybe a -> String
showR MyNothing = "nothing"
showR (MyJust a) = "just something"
