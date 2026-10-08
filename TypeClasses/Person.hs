module TypeClasses.Person where

data Person = MkPerson { nr:: Int, email :: String }

instance Show Person where
    show :: Person -> String
    show p = "MkPerson {nr = " ++ show (nr p) ++ ", email = " ++ show (email p) ++ "}"

instance Eq Person where
    l == r = (nr l) == (nr r)

instance Ord Person where
    l <= r = (nr l) <= (nr r)
