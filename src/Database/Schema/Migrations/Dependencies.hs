module Database.Schema.Migrations.Dependencies
    ( Dependable(..)
    )
where

class Eq a => Dependable a where
    -- |The objects on which a depends.
    depsOf :: a -> [a]

    -- |Return the complete dependency list for the specified object,
    -- including its children and their dependencies recursively, and
    -- including the object itself.  Some notes:
    --
    -- a == last $ fullDeps a
    -- [] == satisfyDeps (fullDeps a) a
    -- True == satisfies (fullDeps a) a
    fullDeps :: a -> [a]
    fullDeps object = (concat $ map fullDeps $ depsOf object) ++ [object]

    -- |Given a list of a in the environment, does that list satisfy
    -- the dependency requirements of an a?  returns whether or not it
    -- does.
    satisfies :: [a] -> a -> Bool
    satisfies env object = and $ map (`elem` env) $ fullDeps object

    -- |Given a list of a (the environment) and an a whose
    -- dependencies must be satisfied, return a list of a which would
    -- satisfy those dependencies, i.e., such that
    --
    -- satisfies (env ++ $ satisfyDeps env a) a = True.
    satisfyDeps :: [a] -> a -> [a]
    satisfyDeps env object = filter (not . (`elem` env)) $ fullDeps object
