module Expression where

data Expr = 
        Div Expr Expr 
        | Sum Expr Expr
        | Sub Expr Expr 
        | Mul Expr Expr 
        | Pow Expr Expr 
        | Expr Int
          deriving (Show)

data ArithmeticError = DivisionByZero | NegativePow
          deriving (Show, Eq)

eval :: Expr -> Either ArithmeticError Int
eval (Expr a)    = Right a
eval (Sum el er) = evalWithOp el er (+) (const False) undefined
eval (Sub el er) = evalWithOp el er (-) (const False) undefined
eval (Mul el er) = evalWithOp el er (*) (const False) undefined
eval (Div el er) = evalWithOp el er div (== 0) DivisionByZero
eval (Pow el er) = evalWithOp el er (^) (< 0) NegativePow

evalWithOp :: Expr -> Expr -> (Int -> Int -> Int) -> (Int -> Bool) -> ArithmeticError -> Either ArithmeticError Int
evalWithOp el er f checker errorType = do
                    lres <- eval el
                    rres <- eval er
                    if checker rres then 
                        Left errorType 
                    else 
                        return (f lres rres)
