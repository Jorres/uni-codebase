-- type User = (Int, String, String)
-- this is called type alias
-- but ExitStatus(Int, String, String) is the same()
-- 
--
-- Type Multiplication : struct with multiple fields
--
-- struct {
--     string s;
--     int a;
-- } int x string;
--
-- Type Sum : std::variant
--
--
-- Constructor and patternmatching are tightly bound together
--
-- Kind of type - type of the type :) 
-- simple types as string, int - kind *
-- data Point2D a = Point2D a a, kind * -> *, because it requires one * substitution to become simple
-- name of constructor can be equal to the name of the type
--
-- you can pattenmatch on constructors
--
-- в общем можно параметризовать тип кайндом, это не тип, а плейсхолдер для типа. На слайдах примеры        
--
data IntResult = Success Int | Failure String

safeDiv :: Int -> Int -> IntResult
safeDiv _ 0 = Failure "Division by zero"
safeDiv x y = Success (div x y)

showResult :: IntResult -> String
showResult (Failure e) = "Failure: " ++ e
showResult (Success n) = "Success: " ++ show n

