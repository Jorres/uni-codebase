 data User = User
    { uid   :: Int
    , login :: String
    }

-- syntax sugar for making a constructor named User and getters
-- for all fields
--
-- you can pattern-match and update records, 55:00
--
-- we can match just a constructor
--
-- wildcards '..'
