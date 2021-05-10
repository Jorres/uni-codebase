module Main where

import NatSpec
import TreeSpec
import DaysOfWeekSpec
import MonoidsSpec
import GroupsSpec
import BreakSpec

main :: IO () 
main = do 
    mainDaysOfWeekSpec
    mainNatSpec
    mainTreeSpec
    mainMonoidsSpec
    mainGroupsSpec
    mainBreakSpec
