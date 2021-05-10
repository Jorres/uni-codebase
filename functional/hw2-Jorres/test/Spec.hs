module Main where

import StringSumSpec
import TreeSpec
import NonEmptySpec
import ExpressionSpec
import MovingAverageSpec
import ParserSpec

main :: IO ()
main = do
    mainStringSumSpec
    mainTreeSpec
    mainNonEmptySpec
    mainExpressionSpec
    mainMovingAverageSpec
    mainParserSpec
