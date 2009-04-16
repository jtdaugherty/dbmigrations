module Database.Schema.Migrations.Dependencies
    ( Dependable(..)
    )
where

class Eq a => Dependable a where
    depsOf :: a -> [a]

    -- |Given a list of a in the environment, does that list satisfy
    -- the dependency requirements of an a?  returns whether or not it
    -- does.
    satisfies :: [a] -> a -> Bool
    satisfies env object = and $ map (`elem` env) $ depsOf object

    -- |Given a list of a (the environment) and an a whose
    -- dependencies must be satisfied, return a list of a which would
    -- satisfy those dependencies, i.e., such that
    --
    -- satisfies (env ++ $ satisfyDeps env a) a = True.
    satisfyDeps :: [a] -> a -> [a]
    satisfyDeps env object = filter (not . (`elem` env)) $ depsOf object
