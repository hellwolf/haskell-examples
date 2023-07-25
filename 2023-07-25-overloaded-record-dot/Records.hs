{-# LANGUAGE DuplicateRecordFields #-}

module Records (Company (name, owner), mkCompany, Person (name)) where

data Person = MkPerson { name :: String }
data Company = MkCompany { name :: String, owner :: Person }

mkCompany companyName ownerName = MkCompany { name = companyName
                                          , owner = MkPerson { name = ownerName } }
