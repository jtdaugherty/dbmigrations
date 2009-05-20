module Common
    ( TestDependable(..)
    )
where

import Database.Schema.Migrations.Dependencies ( Dependable(..) )

instance Dependable TestDependable where
    depId = tdId
    depsOf = tdDeps

data TestDependable = TD { tdId :: String
                         , tdDeps :: [String]
                         }
                      deriving (Show, Eq, Ord)
