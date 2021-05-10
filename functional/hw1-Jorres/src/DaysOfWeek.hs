module DaysOfWeek where

data DaysOfWeek = Monday
                | Tuesday
                | Wednesday
                | Thursday
                | Friday
                | Saturday
                | Sunday
    deriving Show

instance Eq DaysOfWeek where
    (==) Monday Monday       = True
    (==) Tuesday Tuesday     = True
    (==) Wednesday Wednesday = True
    (==) Thursday Thursday   = True
    (==) Friday Friday       = True
    (==) Saturday Saturday   = True
    (==) Sunday Sunday       = True
    (==) _ _                 = False
    (/=) a b = not (a == b)

nextDay :: DaysOfWeek -> DaysOfWeek
nextDay Monday    = Tuesday
nextDay Tuesday   = Wednesday
nextDay Wednesday = Thursday
nextDay Thursday  = Friday
nextDay Friday    = Saturday
nextDay Saturday  = Sunday
nextDay Sunday    = Monday

afterDays :: DaysOfWeek -> Int -> DaysOfWeek
afterDays day 0 = day
afterDays day n = afterDays (nextDay day) (n - 1)

isWeekend :: DaysOfWeek -> Bool
isWeekend Saturday = True
isWeekend Sunday   = True
isWeekend _        = False

daysToParty :: DaysOfWeek -> Int
daysToParty Friday   = 0
daysToParty otherDay = (daysToParty $ nextDay otherDay) + 1
