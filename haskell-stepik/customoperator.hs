module CustomOperator where

infixl 9 |-|

x |-| y = abs (x - y)

